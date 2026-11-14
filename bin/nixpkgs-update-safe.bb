#!/usr/bin/env bb
;; nixpkgs-update-safe.bb - Update Nix flake inputs safely, with rollback.
;;
;; Babashka port of bin/nixpkgs-update-safe. Updates nixpkgs and home-manager,
;; applies with `just hm-switch`, and commits flake.lock with the input dates.
;; On failure, restores flake.lock, adds the bad nixpkgs rev to the skip list
;; and runs hm-switch again with the old lock.
;;
;; Usage:
;;   bin/nixpkgs-update-safe.bb                 # update nixpkgs and home-manager
;;   bin/nixpkgs-update-safe.bb --nixpkgs-only  # update nixpkgs only
;;   bin/nixpkgs-update-safe.bb --dry-run       # report only, change nothing
;;
;; Differences from the bash version:
;; - The flake.lock backup and the error log stay in memory. The log goes to
;;   a temp file only after a rollback, for inspection.
;; - An interrupt (INT/TERM) or an unexpected error rolls back but does not
;;   add the nixpkgs rev to the skip list. Only a failed update or hm-switch
;;   does.
;; - The lastModified date fallback follows the root nixpkgs input, as the
;;   rev lookup does.
;; - Unknown options are an error (exit 1).
;; - No jq or curl dependency.
;; - The binary-cache check counts only derivations that run a build. It asks
;;   nix for each one, so generated files, wrappers and buildEnvs do not count
;;   as compilation. The bash version filters by derivation name.
;; - --dry-run updates flake.lock to check the binary cache, then restores
;;   it. It reports the rev, the date and the derivations a real run would
;;   build, including the older channel release it would fall back to when
;;   the newest nixpkgs needs compiling. The bash version has no such option.
;;
;; Tests: scripts/nixpkgs-update-safe-test.sh (end to end, with mocks) and
;; scripts/nixpkgs-update-safe-test.bb (pure functions).

(ns ^{:clj-kondo/ignore [:namespace-name-mismatch]} nixpkgs-update-safe
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :as cli])
  (:import [java.time Instant LocalDate ZoneOffset]
           [sun.misc Signal SignalHandler]))

;; --- Configuration ---

(def lock-file "flake.lock")
(def skip-list-file ".nix-skip-commits")

(def channel-max-tries
  "Max older channel releases to try when the newest is not cached."
  12)

(def cli-options
  [[nil "--nixpkgs-only" "Update only nixpkgs, not home-manager"]
   ["-n" "--dry-run" "Report what would be built; no switch, no commit"]
   ["-h" "--help" "Show this help"]])

;; --- Output ---

(def ^:private red "\u001b[0;31m")
(def ^:private green "\u001b[0;32m")
(def ^:private yellow "\u001b[1;33m")
(def ^:private nc "\u001b[0m")

(defn- say [s] (println s) (flush))
(defn- info [msg] (say (str green "[INFO]" nc " " msg)))
(defn- warn [msg] (say (str yellow "[WARN]" nc " " msg)))
(defn- error [msg] (say (str red "[ERROR]" nc " " msg)))

(defn short-rev
  "Return the first 8 characters of REV."
  [rev]
  (let [s (str rev)] (subs s 0 (min 8 (count s)))))

(defn truncate
  "Return S cut to at most N characters."
  [s n]
  (if (> (count s) n) (subs s 0 n) s))

(defn- pad-right
  "Pad S with spaces to N characters. Never truncate."
  [s n]
  (let [s (str s)]
    (str s (apply str (repeat (max 0 (- n (count s))) \space)))))

(defn- report-row
  "Return one aligned line of the dry-run report."
  [{:keys [input rev date note]}]
  (str "  " (pad-right input 14) (pad-right rev 10) (pad-right (or date "?") 12) note))

(defn- print-report!
  "Print ROWS, a seq of {:input :rev :date :note}, as an aligned table."
  [rows]
  (say "")
  (say (str "  " (pad-right "input" 14) (pad-right "rev" 10) (pad-right "date" 12) "note"))
  (doseq [row rows] (say (report-row row)))
  (say ""))

;; --- Control flow ---

(def ^:private interrupted (atom false))

(defn- install-signal-handlers!
  "Set a flag on INT and TERM. Do not exit: the main thread sees the flag
   after the child process stops and then rolls back."
  []
  (doseq [sig ["INT" "TERM"]]
    (Signal/handle (Signal. sig)
                   (reify SignalHandler
                     (handle [_ _] (reset! interrupted true))))))

(defn- check-interrupt! []
  (when @interrupted
    (throw (ex-info "Interrupted" {::interrupted true}))))

(defn- halt!
  "Stop the update and exit with CODE. Do not roll back."
  [code]
  (throw (ex-info "Halt" {::exit code})))

(defn- attempt
  "Call F. On an exception, warn and return nil. Use for steps that must
   not stop the update or the rollback."
  [what f]
  (try
    (or (f) true)
    (catch Exception e
      (warn (str "Could not " what ": " (ex-message e)))
      nil)))

;; --- Error log (in memory) ---

(def ^:private log (atom []))

(defn- log! [text]
  (swap! log into (str/split-lines (str text))))

(defn- tmp-dir []
  (or (not-empty (System/getenv "TMPDIR")) "/tmp"))

(defn- save-log!
  "Write the error log to a new temp file. Return its path."
  []
  (let [f (fs/create-temp-file {:dir (tmp-dir) :prefix "nixpkgs-update-safe." :suffix ""})]
    (spit (fs/file f) (str (str/join "\n" @log) "\n"))
    (str f)))

;; --- Processes ---

(defn- exec
  "Run CMD with process OPTS and wait. Return the result map.
   Return exit 127 when CMD cannot start."
  [opts cmd]
  (try
    @(apply p/process opts cmd)
    (catch java.io.IOException e
      {:exit 127 :out (str (ex-message e) "\n")})))

(defn- succeeds? [& cmd]
  (zero? (:exit (exec {:out :string :err :string} cmd))))

(defn- term-cols []
  (let [{:keys [exit out]} (exec {:in :inherit :out :string :err :string} ["tput" "cols"])]
    (or (when (zero? exit) (parse-long (str/trim out))) 80)))

(defn- run-with-progress
  "Run CMD. Show its latest output line in place and add all output to the
   log. Return the exit code of CMD."
  [label & cmd]
  (info label)
  (let [cols (max 1 (- (term-cols) 4))
        proc (try (apply p/process {:in :inherit :err :out} cmd)
                  (catch java.io.IOException e (ex-message e)))
        lines (if (string? proc)
                [proc]
                (with-open [r (io/reader (:out proc))]
                  (reduce (fn [acc line]
                            (when (seq line)
                              (print (str "\r\u001b[K  " (truncate line cols)))
                              (flush))
                            (conj acc line))
                          [] (line-seq r))))
        rc (if (string? proc) 127 (:exit @proc))]
    (swap! log into lines)
    (print "\r\u001b[K")
    (flush)
    (check-interrupt!)
    (when-not (zero? rc)
      (println)
      (error (str "Command failed (exit " rc "). Last output:"))
      (let [tail (take-last 5 (remove empty? lines))]
        (if (seq tail)
          (doseq [line tail] (say (str "  " red ">" nc " " line)))
          (say (str "  " red ">" nc " (no output captured)")))))
    rc))

(defn json-log-lines
  "Flatten a nix json-log-path file: messages plus build log lines
   (resBuildLogLine=101, resPostBuildLogLine=107). Keep lines that are not
   JSON as they are."
  [text]
  (mapcat
   (fn [line]
     (try
       (let [ev (json/parse-string (str/replace-first line #"^@nix " "") true)
             msg (case (:action ev)
                   "msg" (or (:raw_msg ev) (:msg ev))
                   "result" (when (#{101 107} (:type ev)) (get-in ev [:fields 1]))
                   nil)]
         (when (some? msg) (str/split-lines (str msg))))
       (catch Exception _ [line])))
   (str/split-lines text)))

(defn- run-with-bar
  "Run CMD on the real terminal, so nix shows its progress bar. Capture the
   nix log through `json-log-path` and add it to the log. Return the exit code."
  [label & cmd]
  (info label)
  (let [jlog (try (fs/create-temp-file {:dir (tmp-dir) :prefix "nixpkgs-update-safe-json." :suffix ""})
                  (catch java.io.IOException e
                    (warn (str "Cannot create JSON log; output not captured: " (ex-message e)))
                    nil))
        nix-config (->> [(System/getenv "NIX_CONFIG") (when jlog (str "json-log-path = " jlog))]
                        (keep not-empty)
                        (str/join "\n"))
        rc (:exit (exec {:inherit true :extra-env {"NIX_CONFIG" nix-config}} cmd))]
    ;; Log capture must not throw: after a successful switch, an exception
    ;; here would roll back a working system.
    (when jlog
      (attempt "read the JSON log"
               #(when (fs/exists? jlog)
                  (swap! log into (json-log-lines (slurp (fs/file jlog))))
                  nil))
      (attempt "remove the JSON log" #(do (fs/delete-if-exists jlog) nil)))
    (check-interrupt!)
    (when-not (zero? rc)
      (println)
      (error (str "Command failed (exit " rc "). See output above.")))
    rc))

;; --- flake.lock ---

(defn parse-lock [bytes]
  (json/parse-string (String. ^bytes bytes "UTF-8")))

(defn- read-lock []
  (parse-lock (fs/read-all-bytes lock-file)))

(defn nixpkgs-node
  "Return the lock node name of the root nixpkgs input."
  [lock]
  (let [target (get-in lock ["nodes" "root" "inputs" "nixpkgs"])]
    (if (string? target) target "nixpkgs")))

(defn nixpkgs-rev [lock]
  (get-in lock ["nodes" (nixpkgs-node lock) "locked" "rev"]))

(defn input-date
  "Return the lastModified date of lock NODE (UTC, YYYY-MM-DD), or nil."
  [lock node]
  (when-let [ts (get-in lock ["nodes" node "locked" "lastModified"])]
    (str (.toLocalDate (.atZone (Instant/ofEpochSecond (long ts)) ZoneOffset/UTC)))))

(defn nixpkgs-date
  "Return the lastModified date of nixpkgs (UTC, YYYY-MM-DD), or nil."
  [lock]
  (input-date lock (nixpkgs-node lock)))

(defn hm-rev [lock]
  (get-in lock ["nodes" "home-manager" "locked" "rev"]))

;; --- Dates and commit message ---

(defn extract-update-date
  "Return the new date of INPUT from `nix flake update` output lines, e.g.
   • Updated input 'nixpkgs': '...' (2024-01-15) → '...' (2024-01-20)
   Use the last date on the last matching line."
  [lines input]
  (let [needle (str "input '" (str/lower-case input) "'")]
    (->> lines
         (filter #(str/includes? (str/lower-case %) needle))
         (mapcat #(re-seq #"\((\d{4}-\d{2}-\d{2})\)" %))
         (map second)
         last)))

(defn format-date
  "Return the YYYY-MM-DD part of RAW. Return today when RAW is blank."
  [raw]
  (if (str/blank? raw)
    (str (LocalDate/now))
    (truncate raw 10)))

(defn commit-message
  "Make the commit message from PAIRS, a seq of [input date]."
  [pairs]
  (let [pairs (if (seq pairs) pairs [["flake.lock" (str (LocalDate/now))]])]
    (str "nix: update " (str/join " & " (map first pairs))
         " (" (str/join ", " (map second pairs)) ")")))

(defn changes
  "Return the [input date] pairs for a change from OLD-LOCK to NEW-LOCK.
   Dates come from the update output, else from the lock."
  [old-lock new-lock {:keys [nixpkgs-only]}]
  (cond-> []
    (not= (nixpkgs-rev old-lock) (nixpkgs-rev new-lock))
    (conj ["nixpkgs" (format-date (or (extract-update-date @log "nixpkgs")
                                     (nixpkgs-date new-lock)))])

    (and (not nixpkgs-only) (not= (hm-rev old-lock) (hm-rev new-lock)))
    (conj ["home-manager" (format-date (extract-update-date @log "home-manager"))])))

;; --- Skip list ---

(defn- skip-listed?
  "Return true when REV is in the skip list. Return false when the list
   cannot be read, as grep does in the bash version."
  [rev]
  (try
    (and (fs/exists? skip-list-file)
         (contains? (set (str/split-lines (slurp skip-list-file))) rev))
    (catch Exception e
      (warn (str "Cannot read " skip-list-file ": " (ex-message e)))
      false)))

(defn- add-to-skip-list! [rev]
  (let [old (if (fs/exists? skip-list-file) (slurp skip-list-file) "")
        sep (if (or (empty? old) (str/ends-with? old "\n")) "" "\n")]
    (spit skip-list-file (str sep rev "\n") :append true)))

;; --- Binary cache check ---

(defn parse-uncached
  "Return the store paths in the `will be built:` part of nix dry-run OUT,
   without the /nix/store/ prefix."
  [out]
  (->> (str/split-lines out)
       (reduce (fn [{:keys [in? acc]} line]
                 (cond
                   (str/includes? line "will be built:") {:in? true :acc acc}
                   (and in? (str/starts-with? line " ")) {:in? true :acc (conj acc line)}
                   :else {:in? false :acc acc}))
               {:in? false :acc []})
       :acc
       (map #(str/replace % #".*/nix/store/" ""))
       vec))

(defn drv-compiles?
  "Return true when a derivation with ATTRS runs a build. Return false for
   fixed-output fetches and for derivations that only write files, join
   paths, wrap programs or copy prebuilt binaries. Treat unknown attributes
   as a build: over-reporting is safe, under-reporting is not. ATTRS uses
   string keys, as `nix derivation show` returns them."
  [attrs]
  (let [env (or (get attrs "env") {})
        ;; Nix 2.35 reports structured attributes at the top level. Older
        ;; versions put them in env.__json.
        json (when-let [j (get env "__json")]
               (try (json/parse-string j) (catch Exception _ {})))
        sa (merge env json (get attrs "structuredAttrs"))
        phases (str/trim (str (get sa "phases")))]
    (cond
      ;; A fixed-output derivation fetches a file; it does not compile.
      (get-in attrs ["outputs" "out" "hash"]) false
      ;; A fetch or an env builder.
      (str/starts-with? (str (get attrs "builder")) "builtin:") false
      ;; runCommand, writeTextFile, symlinkJoin and buildEnv.
      (or (contains? sa "buildCommand") (contains? sa "text")) false
      ;; A local step that does not unpack a source: it copies, substitutes
      ;; or wraps files. Compiles unpack a source and accept remote builders.
      (and (get sa "dontUnpack") (get sa "preferLocalBuild")) false
      ;; Only unpack, install or similar phases run: nothing compiles.
      (and (seq phases) (not (str/includes? phases "buildPhase"))) false
      :else true)))

(defn- drv-attrs
  "Return the attributes of the derivations at DRV-PATHS. Keys are the store
   path names (nix 2.35) or the full paths (older nix). Return nil when nix
   cannot show them."
  [drv-paths]
  (let [{:keys [exit out]} (exec {:out :string :err :string}
                                 (into ["nix" "derivation" "show"] drv-paths))]
    (when (zero? exit)
      (let [parsed (try (json/parse-string out) (catch Exception _ nil))]
        (or (get parsed "derivations") parsed)))))

(defn compiling-drvs
  "Return the derivations in NAMES that compile something. NAMES are store
   path names without the prefix, as parse-uncached returns them. A
   derivation whose attributes cannot be read counts as a compile."
  [names]
  (if (empty? names)
    []
    (let [path (fn [name] (str "/nix/store/" name))
          derivs (drv-attrs (map path names))
          attrs-of (fn [name] (or (get derivs name) (get derivs (path name))))]
      (filterv (fn [name]
                 (if-let [attrs (attrs-of name)]
                   (drv-compiles? attrs)
                   true))
               names))))

(defn describe-build
  "Return a short description of DRVS, the derivations a dry run would compile."
  [drvs]
  (cond
    (= drvs ::eval-failed) "cache check could not evaluate"
    (empty? drvs) "no compilation"
    (= 1 (count drvs)) "1 derivation would compile"
    :else (str (count drvs) " derivations would compile")))

(defn- hm-attr []
  (let [host (str/trim (:out (exec {:out :string :err :string} ["hostname"])))]
    (str ".#homeConfigurations." (str/replace host "." "") ".activationPackage")))

(defn- uncached-drvs
  "Return the derivations that a dry-run of ATTR would compile, or
   ::eval-failed when the dry-run fails."
  [attr]
  (let [{:keys [exit out]} (exec {:out :string :err :out} ["nix" "build" "--dry-run" attr])]
    (check-interrupt!)
    (if (zero? exit)
      (compiling-drvs (parse-uncached out))
      (do (log! out) ::eval-failed))))

(defn older-releases
  "Return the 12-char revs of channel releases in the release index LISTING
   that are older than NEW-REV, newest first. Return () when NEW-REV is not
   a release."
  [listing new-rev]
  (->> (re-seq #"pre(\d+)\.([0-9a-f]{12})" listing)
       (reduce (fn [m [_ n rev]]
                 (let [n (parse-long n)]
                   (if (contains? m n) m (assoc m n rev))))
               {})
       (sort-by key >)
       (map val)
       (drop-while #(not (str/starts-with? new-rev %)))
       rest))

(defn- channel-candidates
  "Return channel release revs older than NEW-REV, newest first. Source: the
   nixpkgs release index. Hydra has usually built these. Return
   ::index-failed when the index cannot be read."
  [new-rev]
  (let [{:keys [exit out]} (exec {:out :string :err :inherit}
                                 ["nix" "eval" "--raw" (str "github:NixOS/nixpkgs/" new-rev "#lib.version")])]
    (check-interrupt!)
    (if-not (zero? exit)
      (do (warn "Cannot read the nixpkgs version") ::index-failed)
      (let [series (str/join "." (take 2 (str/split (str/trim out) #"\.")))
            url (str "https://nix-releases.s3.amazonaws.com/?delimiter=/&prefix=nixpkgs/nixpkgs-"
                     series "pre&max-keys=1000")
            ;; The request runs in-process, so Ctrl-C cannot stop it. Time out.
            resp (try (http/get url {:throw false :timeout 30000})
                      (catch Exception e {:status (ex-message e)}))]
        (if (= 200 (:status resp))
          (older-releases (:body resp) new-rev)
          (do (warn (str "Cannot read the release index: " (or (not-empty (str (:status resp)))
                                                               "no response")))
              ::index-failed))))))

(defn- find-cached-release
  "Lock nixpkgs to each older channel release until one is fully cached.
   Return {:rev rev :tried [{:rev :date :drvs} ...] :index-failed? bool};
   :rev is nil when no release is fully cached."
  [attr new-rev old-rev]
  (let [candidates (channel-candidates new-rev)
        index-failed? (= candidates ::index-failed)]
    (loop [[rev & more] (when-not index-failed? candidates)
           tries 1
           tried []]
      (let [done {:rev nil :tried tried :index-failed? index-failed?}]
        (cond
          (nil? rev) done
          ;; Reached the previously applied rev: nothing newer is cached.
          (and old-rev (str/starts-with? old-rev rev)) done
          (> tries channel-max-tries) done
          :else
          (if-not (zero? (run-with-progress
                          (format "Trying channel release %s (%d/%d)..." rev tries channel-max-tries)
                          "nix" "flake" "lock" "--override-input" "nixpkgs" (str "github:NixOS/nixpkgs/" rev)))
            (recur more (inc tries) tried)
            (let [drvs (uncached-drvs attr)
                  entry (when (not= drvs ::eval-failed)
                          {:rev rev :date (nixpkgs-date (read-lock)) :drvs drvs})
                  tried (if entry (conj tried entry) tried)]
              (if (and entry (empty? drvs))
                {:rev rev :tried tried :index-failed? false}
                (recur more (inc tries) tried)))))))))

;; --- Rollback ---

(defn- restore-lock! [state]
  (fs/write-bytes lock-file (:backup @state)))

(defn- rollback!
  "Restore flake.lock and the previous configuration. Run only once.
   Add the new nixpkgs rev to the skip list when SKIP-LIST? is true.
   In a dry run, only restore flake.lock."
  [state {:keys [skip-list?]}]
  (when-not (:rolled-back? @state)
    (swap! state assoc :rolled-back? true)
    (warn (if (:dry-run? @state) "Restoring flake.lock (dry run)..." "Rolling back flake.lock..."))
    ;; Restore the file and the git index first.
    (attempt "restore flake.lock" #(restore-lock! state))
    (exec {:out :string :err :string} ["git" "restore" "--staged" lock-file])
    (when-not (:dry-run? @state)
      ;; Nothing in this step may stop the switch back below.
      (attempt
       "update the skip list"
       #(let [{:keys [old-rev new-rev]} @state
              disk-full? (some (fn [line] (re-find #"(?i)no space left on device" line)) @log)]
          (when disk-full?
            (warn "Detected 'no space left on device'; will not add nixpkgs rev to skip list"))
          (when (and skip-list? (not disk-full?) (some? new-rev) (not= new-rev old-rev)
                     (not (skip-listed? new-rev)))
            (add-to-skip-list! new-rev)
            (warn (str "Added problematic nixpkgs commit to skip list: " new-rev)))))
      (warn "Running hm-switch to restore previous configuration...")
      (if (zero? (:exit (exec {:inherit true} ["just" "hm-switch"])))
        (info "Successfully restored previous configuration")
        (do (error "Failed to restore previous configuration with hm-switch")
            (error "You may need to manually run 'just hm-switch' to fix your system"))))
    (when (seq @log)
      (when-let [path (attempt "write the error log" save-log!)]
        (warn (str "Full error log: " path))))
    (info (if (:dry-run? @state) "Dry run stopped; flake.lock restored" "Rollback completed"))))

(defn- lock-changed?
  "Return true when flake.lock differs from the backup. A missing or
   unreadable lock counts as changed, as with `diff` in the bash version."
  [state]
  (when-let [backup (:backup @state)]
    (try
      (not (java.util.Arrays/equals ^bytes backup ^bytes (fs/read-all-bytes lock-file)))
      (catch Exception _ true))))

;; --- Dry run ---

(defn dry-run-rows
  "Return the candidate rows for a dry run: the rev in use, the latest
   nixpkgs and each older release tried. CHOSEN is the lock a real run
   would use; OLD-LOCK is the lock before the update."
  [state old-lock chosen]
  (let [{:keys [rev date drvs]} (:cache-checked @state)
        chosen-rev (nixpkgs-rev chosen)]
    (concat
     [{:input "nixpkgs" :rev (short-rev (nixpkgs-rev old-lock))
       :date (nixpkgs-date old-lock) :note "in use"}]

     ;; The newest nixpkgs, when it differs from the rev in use.
     (when (and rev (not= rev (nixpkgs-rev old-lock)))
       [{:input "" :rev (short-rev rev) :date date
         :note (str "latest, " (describe-build drvs))}])

     ;; Older channel releases tried and rejected, newest first. The chosen
     ;; release is left out: the summary reports it.
     (->> (:cache-tried @state)
          (remove #(and chosen-rev (:rev %) (str/starts-with? chosen-rev (:rev %))))
          (map (fn [{:keys [rev date drvs]}]
                 {:input "" :rev (short-rev rev) :date date
                  :note (str "older release, " (describe-build drvs))}))))))

(defn dry-run-summary
  "Return the conclusion lines for a dry run: the input revs a real run
   would use, or the rev it would keep, and the commit message it would write."
  [state old-lock chosen {:keys [nixpkgs-only old-hm]}]
  (let [{:keys [rev drvs]} (:cache-checked @state)
        chosen-rev (nixpkgs-rev chosen)
        chosen-hm (hm-rev chosen)
        old-hm (or old-hm (hm-rev old-lock))]
    (if (:keep-current? @state)
      [(str "Would keep nixpkgs " chosen-rev " (" (or (nixpkgs-date chosen) "unknown date") "): "
            (if (:index-failed? @state)
              "could not read the release index, so no older release was tried"
              "no fully cached release newer than it"))]
      (concat
       [(str "Would use nixpkgs " chosen-rev " (" (or (nixpkgs-date chosen) "unknown date") "): "
             (if (= chosen-rev rev) (describe-build drvs) "no compilation"))]

       (when (and (not nixpkgs-only) (not= old-hm chosen-hm))
         [(str "Would use home-manager " chosen-hm
               " (" (or (input-date chosen "home-manager") "unknown date") ")"
               ", was " (short-rev old-hm))])

       [(str "Would commit: " (commit-message (changes old-lock chosen
                                                       {:nixpkgs-only nixpkgs-only})))]))))

(defn- print-dry-run!
  "Print the candidates and the conclusion, then restore flake.lock.
   Change nothing else."
  [state {:keys [old-lock] :as opts}]
  (let [chosen (read-lock)]
    (print-report! (dry-run-rows state old-lock chosen))
    (doseq [line (dry-run-summary state old-lock chosen opts)] (info line))
    (attempt "restore flake.lock" #(do (restore-lock! state) true))
    (exec {:out :string :err :string} ["git" "restore" "--staged" lock-file])
    (if (succeeds? "git" "diff" "--quiet" lock-file)
      (info "Dry run complete: flake.lock restored, nothing switched or committed")
      (warn "flake.lock differs from HEAD after the dry run; check git status"))))

;; --- Steps ---

(defn- preflight! []
  (letfn [(fail [msg] (error msg) (halt! 1))]
    (when-not (succeeds? "git" "rev-parse" "--git-dir") (fail "Not in a git repository"))
    (when-not (fs/exists? lock-file) (fail "flake.lock not found"))
    (when-not (succeeds? "git" "diff" "--quiet" lock-file) (fail "flake.lock has uncommitted changes"))
    (doseq [cmd ["just" "nix"]]
      (when-not (fs/which cmd) (fail (str cmd " not found"))))
    (when-not (succeeds? "git" "diff" "--cached" "--quiet" lock-file)
      (fail "flake.lock is staged. Please unstage it first."))))

(defn- update-inputs! [state nixpkgs-only]
  (let [rc (if nixpkgs-only
             (run-with-progress "Updating nixpkgs..." "nix" "flake" "update" "nixpkgs")
             (run-with-progress "Updating flake inputs..." "just" "nixpkgs-update-flake"))]
    (when-not (zero? rc)
      (error (if nixpkgs-only "nix flake update failed" "nixpkgs-update-flake failed"))
      (rollback! state {:skip-list? true})
      (halt! 1))))

(defn- ensure-cached!
  "Check that the new nixpkgs is in the binary cache. If not, step back
   through channel releases to the newest one that is fully cached. Return
   the nixpkgs rev to use. Record each check in STATE for the dry-run report."
  [state new-rev old-rev]
  (info (str "Checking binary cache for nixpkgs " (short-rev new-rev) "..."))
  (let [attr (hm-attr)
        date (nixpkgs-date (read-lock))
        drvs (uncached-drvs attr)]
    (swap! state assoc :cache-checked {:rev new-rev :date date :drvs drvs})
    (cond
      (= drvs ::eval-failed)
      (do (warn "Cache check failed to evaluate; continuing without it") new-rev)

      (empty? drvs) new-rev

      :else
      (do
        (warn (str (count drvs) " derivation(s) would build from source:"))
        (doseq [d (take 5 drvs)] (say (str "    " d)))
        (if (= new-rev old-rev)
          (do (warn "nixpkgs unchanged; nothing to step back to. Continuing.") new-rev)
          (let [{found :rev tried :tried index-failed? :index-failed?}
                (find-cached-release attr new-rev old-rev)]
            (swap! state assoc :cache-tried tried :index-failed? index-failed?)
            (if found
              (let [rev (nixpkgs-rev (read-lock))]
                (info (str "Using cached nixpkgs release " found " (" (short-rev rev) ")"))
                rev)
              (do
                (if index-failed?
                  (warn "Cannot read the release index; cannot look for a cached release. Keeping current flake.lock.")
                  (do (warn "No newer fully cached nixpkgs found. Keeping current flake.lock.")
                      (warn "Not skip-listed: retry later, once Hydra catches up.")))
                (swap! state assoc :keep-current? true)
                (restore-lock! state)
                ;; A dry run reports this instead of stopping silently.
                (if (:dry-run? @state) old-rev (halt! 0))))))))))

(defn- commit!
  "Stage and commit flake.lock. Stop on an interrupt around each git step.
   Set :committed? in STATE when the commit succeeds."
  [state msg]
  (let [manual (fn []
                 (error "System was updated successfully, but the commit failed.")
                 (error "flake.lock is staged with the new update. Finish the commit manually:")
                 (error (str "  git commit -m \"" msg "\""))
                 (halt! 1))
        git! (fn [& args]
               (check-interrupt!)
               (zero? (:exit (exec {:inherit true} (cons "git" args)))))
        added? (git! "add" lock-file)]
    (check-interrupt!)
    (when-not added? (manual))
    (let [committed? (git! "commit" "-m" msg)]
      ;; Record the commit before the interrupt check, so the message is true.
      (when committed? (swap! state assoc :committed? true))
      (check-interrupt!)
      (when-not committed? (manual)))))

(defn- update!
  "Run the update. Return normally on success. Throw ::exit or
   ::interrupted ex-info to stop."
  [state {:keys [nixpkgs-only dry-run]}]
  (preflight!)
  (info (if dry-run
          "Starting dry run; flake.lock is restored afterwards"
          "Starting safe Nix flake update..."))
  (let [backup (fs/read-all-bytes lock-file)
        old-lock (parse-lock backup)
        old-rev (nixpkgs-rev old-lock)
        old-hm (hm-rev old-lock)]
    (swap! state assoc :backup backup :old-rev old-rev :dry-run? dry-run)
    (update-inputs! state nixpkgs-only)
    (when (succeeds? "git" "diff" "--quiet" lock-file)
      (info "flake.lock unchanged, nothing to do")
      (halt! 0))
    (info "flake.lock changed, proceeding with checks...")
    (let [new-rev (nixpkgs-rev (read-lock))]
      (swap! state assoc :new-rev new-rev)
      (when (and new-rev (skip-listed? new-rev))
        (warn (str "Nixpkgs commit " new-rev " is in skip list (known problematic)"))
        (restore-lock! state)
        (warn (str "Remove commit from " skip-list-file " to retry."))
        (halt! 0))
      (let [new-rev (ensure-cached! state new-rev old-rev)]
        (swap! state assoc :new-rev new-rev)
        (when dry-run
          (print-dry-run! state {:nixpkgs-only nixpkgs-only :old-lock old-lock :old-hm old-hm})
          (halt! 0))
        (when-not (zero? (run-with-bar "Running hm-switch..." "just" "hm-switch"))
          (error "hm-switch failed")
          (rollback! state {:skip-list? true})
          (halt! 1))
        ;; The system now runs the new configuration. From here on, never
        ;; roll back the system; only git-side recovery is correct.
        (swap! state assoc :applied? true)
        (when (not= old-rev new-rev)
          (info (str "Nixpkgs updated: " (short-rev old-rev) " → " (short-rev new-rev))))
        (when (and (not nixpkgs-only) (not= old-hm (hm-rev (read-lock))))
          (info (str "Home-manager updated: " (short-rev old-hm) " → "
                     (short-rev (hm-rev (read-lock))))))
        (let [new-lock (read-lock)
              msg (commit-message (changes old-lock new-lock {:nixpkgs-only nixpkgs-only}))]
          (info (str "Commit message: " msg))
          (commit! state msg)
          (info "✅ Successfully updated and committed flake.lock")
          (info (str "Commit: " (str/trim (:out (exec {:out :string :err :inherit}
                                                      ["git" "rev-parse" "--short" "HEAD"]))))))))))

(defn run
  "Run the update with parsed OPTIONS. Return the exit code."
  [options]
  (let [state (atom {})
        roll-back-if-needed! #(when (and (not (:applied? @state)) (lock-changed? state))
                                (rollback! state {:skip-list? false}))]
    (try
      (update! state options)
      0
      (catch Exception e
        (let [data (ex-data e)]
          (cond
            (contains? data ::exit) (::exit data)

            (::interrupted data)
            (do (println)
                (warn "Interrupted by signal")
                (cond
                  (:committed? @state) (info "System is updated and flake.lock is committed.")
                  (:applied? @state) (warn "System is updated, but flake.lock is not committed.")
                  :else (roll-back-if-needed!))
                130)

            :else
            (do (error (str "Unexpected error: " (or (ex-message e) (str e))))
                (roll-back-if-needed!)
                1)))))))

(defn- usage [summary]
  (str/join "\n" ["Usage: nixpkgs-update-safe.bb [options]"
                  ""
                  "Update nixpkgs (and home-manager), apply with hm-switch, and commit"
                  "flake.lock. Roll back on failure."
                  ""
                  "With --dry-run, only report: the rev, the date and the"
                  "derivations a real run would build. flake.lock is restored, and"
                  "nothing is switched or committed."
                  ""
                  "Options:"
                  summary]))

(defn -main [& args]
  (let [{:keys [options errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (:help options) (do (say (usage summary)) (System/exit 0))
      errors (do (doseq [e errors] (error e))
                 (say (usage summary))
                 (System/exit 1))
      :else (do (install-signal-handlers!)
                (System/exit (run options))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
