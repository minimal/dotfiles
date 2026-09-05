#!/usr/bin/env bb
;; bump-beads.bb - refresh nixpkgs/overlays/beads-sources.json from the latest
;; GitHub releases of the beads packages (beads_rust -> br, beads_viewer -> bv).
;;
;; Deterministic, zero Clojure deps: uses gh (with its built-in --jq) for
;; release metadata, nix eval to read the existing JSON back, and
;; nix-prefetch-url --unpack to re-hash artifacts whose version changed.
;; Unchanged packages keep their stored URLs + hashes verbatim (upstream may
;; change asset naming between releases, e.g. bv switched to versioned names).
;;
;; Usage:
;;   bump-beads.bb                 # update all packages in the manifest
;;   bump-beads.bb br              # update one package (others kept as-is)
;;   bump-beads.bb --check         # report staleness, change nothing (exit 1 if stale)
;;   bump-beads.bb --commit        # commit the regenerated JSON afterwards
;;
;; Adding a package: extend `manifest` + the mkBeadsPkg calls in
;; nixpkgs/overlays/bins.nix; first run without a package arg repopulates.

(ns bump-beads
  (:require [babashka.process :refer [shell]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def repo-root
  (-> *file* io/file .getCanonicalFile .getParentFile .getParentFile))

(def sources-path
  (str (io/file repo-root "nixpkgs" "overlays" "beads-sources.json")))

(def sources-rel-path "nixpkgs/overlays/beads-sources.json")

(def manifest
  "Per package: GitHub repo and the CURRENT release asset name per nix system.
   %s in an asset template is the version (v-prefix stripped). Only consulted
   when a package actually bumps; stored entries are copied verbatim."
  {"br" {:repo "Dicklesworthstone/beads_rust"
         :assets {"aarch64-darwin" "br-%s-darwin_arm64.tar.gz"
                  "aarch64-linux" "br-%s-linux_musl_amd64.tar.gz"
                  "x86_64-darwin" "br-%s-darwin_amd64.tar.gz"
                  "x86_64-linux" "br-%s-linux_musl_amd64.tar.gz"}}
   "bv" {:repo "Dicklesworthstone/beads_viewer"
         :assets {"aarch64-darwin" "bv_%s_darwin_arm64.tar.gz"
                  "aarch64-linux" "bv_%s_linux_arm64.tar.gz"
                  "x86_64-darwin" "bv_%s_darwin_amd64.tar.gz"
                  "x86_64-linux" "bv_%s_linux_amd64.tar.gz"}}})

(def usage
  (str "Usage: bump-beads.bb [--check] [--commit] [PKG...]\n\n"
       "Refresh " sources-rel-path " for PKG (default: all).\n"
       "  --check   report stale packages, change nothing, exit 1 if stale\n"
       "  --commit  git commit the regenerated JSON (path-limited commit)\n"))

(defn die [& msg]
  (binding [*out* *err*] (apply println msg))
  (System/exit 1))

(defn sh-out
  "Run a shell command string, return trimmed stdout; throw with stderr on failure."
  [cmd]
  (let [{:keys [exit out err]} @(shell {:out :string :err :string} cmd)]
    (when-not (zero? exit)
      (throw (ex-info (str "command failed (exit " exit "): " cmd)
                      {:stderr err :stdout out})))
    (str/trim out)))

(defn- strip-v [tag] (str/replace-first tag #"^v" ""))

(defn- latest-release [repo]
  ;; [version #{asset-name}]
  [(-> (sh-out (str "gh api repos/" repo "/releases/latest --jq .tag_name")) strip-v)
   (->> (sh-out (str "gh api repos/" repo "/releases/latest --jq \".assets[].name\""))
        str/split-lines
        set)])

(defn- nix-eval-raw [expr]
  (sh-out (str "nix eval --raw --impure --expr '" expr "'")))

(defn- json-src []
  (str "(builtins.fromJSON (builtins.readFile " sources-path "))"))

(defn- stored-pkgs []
  (when (.exists (io/file sources-path))
    (->> (nix-eval-raw (str "(builtins.concatStringsSep \"\\n\" (builtins.attrNames " (json-src) "))"))
         str/split-lines
         (remove str/blank?)
         sort)))

(defn- stored-version [pkg]
  (try (nix-eval-raw (str "(" (json-src) ")." pkg ".version"))
       (catch Exception _ nil)))

(defn- stored-entry [pkg]
  ;; existing version + per-system url/sha256, verbatim; dies if pkg absent
  (let [version (or (stored-version pkg)
                    (die (str pkg ": not present in " sources-rel-path
                              " - run without a package argument to repopulate")))
        lines (-> (nix-eval-raw
                   (str "(let e = (" (json-src) ")." pkg
                        "; in builtins.concatStringsSep \"\\n\" (map (s: s.sha256 + \"\\t\" + s.url) (builtins.attrValues e.sources)))"))
                  str/split-lines)
        systems (sort (keys (get-in manifest [pkg :assets])))]
    (when-not (= (count lines) (count systems))
      (die (str pkg ": stored entries out of sync with the manifest, refusing to rewrite")))
    {:version version
     :systems (into (sorted-map)
                    (map vector
                         systems
                         (map (fn [l]
                                (let [[sha url] (str/split l #"\t")]
                                  {:sha256 sha :url url}))
                              lines)))}))

(defn- sri-sha256 [url]
  ;; base32 hash of the UNPACKED tree, converted to the SRI form fetchzip wants
  (-> (sh-out (str "nix-prefetch-url --unpack '" url "'"))
      (as-> b32 (sh-out (str "nix hash convert --hash-algo sha256 --to sri " b32)))))

(defn- asset-url [pkg version system]
  (let [{:keys [repo assets]} (manifest pkg)]
    (str "https://github.com/" repo "/releases/download/v" version
         "/" (format (get assets system) version))))

(defn- json-str [s]
  (str "\"" (-> s (str/replace "\\" "\\\\") (str/replace "\"" "\\\"")) "\""))

(defn- emit-json
  "Pretty-print a tree of string-keyed maps / strings: sorted keys, 2-space
   indent, stable output so repeat runs produce byte-identical files."
  [x]
  (cond
    (string? x) (json-str x)
    (map? x) (if (empty? x)
               "{}"
               (str "{\n"
                    (str/join ",\n"
                              (map (fn [[k v]]
                                     (str "  " (json-str k) ": "
                                          (str/replace (emit-json v) "\n" "\n  ")))
                                   (into (sorted-map) x)))
                    "\n}"))
    :else (throw (ex-info "unsupported value in generated JSON" {:value x}))))

(defn- render-entry [{:keys [version systems]}]
  {"sources" (into (sorted-map)
                   (for [[system {:keys [sha256 url]}] systems]
                     [system {"sha256" sha256 "url" url}]))
   "version" version})

(defn- bumped-entry [pkg latest [assets]]
  ;; build a fresh entry: assert asset presence, then prefetch all systems
  (let [systems (sort (keys (get-in manifest [pkg :assets])))]
    (doseq [system systems]
      (let [asset (format (get-in manifest [pkg :assets system]) latest)]
        (when-not (contains? assets asset)
          (die (str pkg " " latest ": release has no asset " asset
                    " - upstream naming may have changed; update the manifest in "
                    (.. (io/file *file*) getName))))))
    (println (str "  " pkg ": prefetching " (count systems) " artifacts..."))
    {:bumped true
     :version latest
     :systems (into (sorted-map)
                    (for [system systems]
                      (let [url (asset-url pkg latest system)]
                        [system {:sha256 (sri-sha256 url) :url url}])))}))

(defn -main [& _]
  (let [args (or *command-line-args* [])
        flags (set (filter #(str/starts-with? % "--") args))
        pos (distinct (remove flags args))]
    (when (or (flags "-h") (flags "--help"))
      (print usage)
      (System/exit 0))
    (doseq [f (remove #{"--check" "--commit"} flags)]
      (die "unknown flag " f ". " usage))
    (doseq [p pos]
      (when-not (manifest p)
        (die "unknown package " p ". known: " (str/join " " (sort (keys manifest))))))
    (let [all-pkgs (sort (keys manifest))
          targets (set (if (seq pos) pos all-pkgs))
          check? (flags "--check")
          original (when (.exists (io/file sources-path)) (slurp sources-path))]
      ;; sanity: the file's package set must match the manifest exactly
      (when-let [existing (stored-pkgs)]
        (when-not (= existing all-pkgs)
          (die sources-rel-path " packages [" (str/join " " existing)
               "] != manifest [" (str/join " " all-pkgs)
               "] - add new packages to the manifest in " (.. (io/file *file*) getName))))
      (cond
        ;; --check: report and leave everything untouched
        check?
        (let [stale (boolean (seq (keep (fn [pkg]
                            (when (targets pkg)
                              (let [[latest _] (latest-release (get-in manifest [pkg :repo]))
                                    cur (stored-version pkg)]
                                (if (= latest cur)
                                  (do (println (str pkg " up to date (" cur ")")) nil)
                                  (do (println (str pkg " stale: " (or cur "absent") " -> " latest)) true)))))
                          all-pkgs)))]
          (System/exit (if stale 1 0)))

        :else
        (let [plan (into (sorted-map)
                         (map (fn [pkg]
                                [pkg (if (targets pkg)
                                       (let [[latest assets] (latest-release (get-in manifest [pkg :repo]))]
                                         (if (= latest (stored-version pkg))
                                           (do (println (str pkg " up to date (" latest ")"))
                                               (stored-entry pkg))
                                           (bumped-entry pkg latest [assets])))
                                       (stored-entry pkg))])
                              all-pkgs))
              written (str (emit-json (into (sorted-map)
                                            (map (fn [[pkg entry]] [pkg (render-entry entry)])
                                                 plan)))
                           "\n")]
          (if (= written original)
            (println "nothing to do")
            (do
              (spit sources-path written)
              ;; guard: nix itself must parse the result and agree on every version
              (try
                (doseq [[pkg {:keys [version]}] plan]
                  (when-not (= (stored-version pkg) version)
                    (throw (ex-info (str "version mismatch for " pkg) {}))))
                (catch Exception e
                  (if original (spit sources-path original) (io/delete-file sources-path))
                  (die "nix rejected the generated JSON, reverted: " (.getMessage e))))
              (println (str "wrote " sources-rel-path))
              (when (flags "--commit")
                (let [msg (str "chore(nix): bump beads-sources "
                               (str/join ", "
                                         (keep (fn [[pkg {:keys [bumped version]}]]
                                                 (when bumped
                                                   (str pkg " -> " version)))
                                               plan)))]
                  (sh-out (str "git -C " repo-root " add -- " sources-rel-path))
                  (sh-out (str "git -C " repo-root " commit -o -m '" msg "' -- "
                               sources-rel-path))
                  (println (str "committed: " msg)))))))))))

(apply -main *command-line-args*)
