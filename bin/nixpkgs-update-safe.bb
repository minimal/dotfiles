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

(def local-drv-re
  "Derivations that always build locally (never cached). Anything else a
   dry-run would build is a binary-cache miss."
  #"^[a-z0-9]{32}-(home-manager-|hm_|hm-putter|activation-script)")

(def channel-max-tries
  "Max older channel releases to try when the newest is not cached."
  12)

(def cli-options
  [[nil "--nixpkgs-only" "Update only nixpkgs, not home-manager"]
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
                        (str/join "\n"))]
    (try
      (let [rc (:exit (exec {:inherit true :extra-env {"NIX_CONFIG" nix-config}} cmd))]
        (when (and jlog (fs/exists? jlog))
          (swap! log into (json-log-lines (slurp (fs/file jlog)))))
        (check-interrupt!)
        (when-not (zero? rc)
          (println)
          (error (str "Command failed (exit " rc "). See output above.")))
        rc)
      (finally
        (when jlog (fs/delete-if-exists jlog))))))

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

(defn nixpkgs-date
  "Return the lastModified date of nixpkgs (UTC, YYYY-MM-DD), or nil."
  [lock]
  (when-let [ts (get-in lock ["nodes" (nixpkgs-node lock) "locked" "lastModified"])]
    (str (.toLocalDate (.atZone (Instant/ofEpochSecond (long ts)) ZoneOffset/UTC)))))

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
  "Make the commit message from CHANGES, a seq of [input date]."
  [changes]
  (let [changes (if (seq changes) changes [["flake.lock" (str (LocalDate/now))]])]
    (str "nix: update " (str/join " & " (map first changes))
         " (" (str/join ", " (map second changes)) ")")))

;; --- Skip list ---

(defn- skip-listed? [rev]
  (and (fs/exists? skip-list-file)
       (contains? (set (str/split-lines (slurp skip-list-file))) rev)))

(defn- add-to-skip-list! [rev]
  (let [old (if (fs/exists? skip-list-file) (slurp skip-list-file) "")
        sep (if (or (empty? old) (str/ends-with? old "\n")) "" "\n")]
    (spit skip-list-file (str sep rev "\n") :append true)))

;; --- Binary cache check ---

(defn parse-uncached
  "Return the store paths in the `will be built:` part of nix dry-run OUT,
   without the always-local ones."
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
       (remove #(re-find local-drv-re %))
       vec))

(defn- hm-attr []
  (let [host (str/trim (:out (exec {:out :string :err :string} ["hostname"])))]
    (str ".#homeConfigurations." (str/replace host "." "") ".activationPackage")))

(defn- uncached-drvs
  "Return the derivations that a dry-run of ATTR would build, or
   ::eval-failed when the dry-run fails."
  [attr]
  (let [{:keys [exit out]} (exec {:out :string :err :out} ["nix" "build" "--dry-run" attr])]
    (check-interrupt!)
    (if (zero? exit)
      (parse-uncached out)
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
   nixpkgs release index. Hydra has usually built these."
  [new-rev]
  (let [{:keys [exit out]} (exec {:out :string :err :inherit}
                                 ["nix" "eval" "--raw" (str "github:NixOS/nixpkgs/" new-rev "#lib.version")])]
    (check-interrupt!)
    (if-not (zero? exit)
      (do (warn "Cannot read the nixpkgs version") ())
      (let [series (str/join "." (take 2 (str/split (str/trim out) #"\.")))
            url (str "https://nix-releases.s3.amazonaws.com/?delimiter=/&prefix=nixpkgs/nixpkgs-"
                     series "pre&max-keys=1000")
            ;; The request runs in-process, so Ctrl-C cannot stop it. Time out.
            resp (try (http/get url {:throw false :timeout 30000})
                      (catch Exception e {:status (ex-message e)}))]
        (if (= 200 (:status resp))
          (older-releases (:body resp) new-rev)
          (do (warn (str "Cannot read the release index: " (:status resp))) ()))))))

(defn- find-cached-release
  "Lock nixpkgs to each older channel release until one is fully cached.
   Return its short rev, or nil."
  [attr new-rev old-rev]
  (loop [[rev & more] (channel-candidates new-rev)
         tries 1]
    (cond
      (nil? rev) nil
      ;; Reached the previously applied rev: nothing newer is cached.
      (and old-rev (str/starts-with? old-rev rev)) nil
      (> tries channel-max-tries) nil
      (and (zero? (run-with-progress
                   (format "Trying channel release %s (%d/%d)..." rev tries channel-max-tries)
                   "nix" "flake" "lock" "--override-input" "nixpkgs" (str "github:NixOS/nixpkgs/" rev)))
           (let [drvs (uncached-drvs attr)]
             (and (not= drvs ::eval-failed) (empty? drvs))))
      rev
      :else (recur more (inc tries)))))

;; --- Rollback ---

(defn- attempt
  "Call F. On an exception, warn and return nil, so a failed step does not
   stop the rollback."
  [what f]
  (try
    (or (f) true)
    (catch Exception e
      (warn (str "Could not " what ": " (ex-message e)))
      nil)))

(defn- restore-lock! [state]
  (fs/write-bytes lock-file (:backup @state)))

(defn- rollback!
  "Restore flake.lock and the previous configuration. Run only once.
   Add the new nixpkgs rev to the skip list when SKIP-LIST? is true."
  [state {:keys [skip-list?]}]
  (when-not (:rolled-back? @state)
    (swap! state assoc :rolled-back? true)
    (warn "Rolling back flake.lock...")
    ;; Restore the file and the git index first.
    (attempt "restore flake.lock" #(restore-lock! state))
    (exec {:out :string :err :string} ["git" "restore" "--staged" lock-file])
    (let [{:keys [old-rev new-rev]} @state
          disk-full? (some #(re-find #"(?i)no space left on device" %) @log)]
      (when disk-full?
        (warn "Detected 'no space left on device'; will not add nixpkgs rev to skip list"))
      (when (and skip-list? (not disk-full?) (some? new-rev) (not= new-rev old-rev)
                 (not (skip-listed? new-rev))
                 (attempt "update the skip list" #(add-to-skip-list! new-rev)))
        (warn (str "Added problematic nixpkgs commit to skip list: " new-rev))))
    (warn "Running hm-switch to restore previous configuration...")
    (if (zero? (:exit (exec {:inherit true} ["just" "hm-switch"])))
      (info "Successfully restored previous configuration")
      (do (error "Failed to restore previous configuration with hm-switch")
          (error "You may need to manually run 'just hm-switch' to fix your system")))
    (when (seq @log)
      (when-let [path (attempt "write the error log" save-log!)]
        (warn (str "Full error log: " path))))
    (info "Rollback completed")))

(defn- lock-changed? [state]
  (let [backup (:backup @state)]
    (and backup
         (fs/exists? lock-file)
         (not (java.util.Arrays/equals ^bytes backup ^bytes (fs/read-all-bytes lock-file))))))

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
   the nixpkgs rev to use."
  [state new-rev old-rev]
  (info (str "Checking binary cache for nixpkgs " (short-rev new-rev) "..."))
  (let [attr (hm-attr)
        drvs (uncached-drvs attr)]
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
          (if-let [found (find-cached-release attr new-rev old-rev)]
            (let [rev (nixpkgs-rev (read-lock))]
              (info (str "Using cached nixpkgs release " found " (" (short-rev rev) ")"))
              rev)
            (do
              (warn "No newer fully cached nixpkgs found. Keeping current flake.lock.")
              (warn "Not skip-listed: retry later, once Hydra catches up.")
              (restore-lock! state)
              (halt! 0))))))))

(defn- commit! [msg]
  (let [manual (fn []
                 (error "System was updated successfully, but the commit failed.")
                 (error "flake.lock is staged with the new update. Finish the commit manually:")
                 (error (str "  git commit -m \"" msg "\""))
                 (halt! 1))]
    (when-not (zero? (:exit (exec {:inherit true} ["git" "add" lock-file])))
      (manual))
    (when-not (zero? (:exit (exec {:inherit true} ["git" "commit" "-m" msg])))
      (manual))))

(defn- update!
  "Run the update. Return normally on success. Throw ::exit or
   ::interrupted ex-info to stop."
  [state {:keys [nixpkgs-only]}]
  (preflight!)
  (info "Starting safe Nix flake update...")
  (let [backup (fs/read-all-bytes lock-file)
        old-lock (parse-lock backup)
        old-rev (nixpkgs-rev old-lock)
        old-hm (hm-rev old-lock)]
    (swap! state assoc :backup backup :old-rev old-rev)
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
        (when-not (zero? (run-with-bar "Running hm-switch..." "just" "hm-switch"))
          (error "hm-switch failed")
          (rollback! state {:skip-list? true})
          (halt! 1))
        ;; The system now runs the new configuration. From here on, never
        ;; roll back the system; only git-side recovery is correct.
        (swap! state assoc :applied? true)
        (let [new-lock (read-lock)
              new-hm (hm-rev new-lock)
              changes (cond-> []
                        (not= old-rev new-rev)
                        (conj (do (info (str "Nixpkgs updated: " (short-rev old-rev) " → " (short-rev new-rev)))
                                  ["nixpkgs" (format-date (or (extract-update-date @log "nixpkgs")
                                                              (nixpkgs-date new-lock)))]))

                        (and (not nixpkgs-only) (not= old-hm new-hm))
                        (conj (do (info (str "Home-manager updated: " (short-rev old-hm) " → " (short-rev new-hm)))
                                  ["home-manager" (format-date (extract-update-date @log "home-manager"))])))
              msg (commit-message changes)]
          (info (str "Commit message: " msg))
          (commit! msg)
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
                (roll-back-if-needed!)
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
