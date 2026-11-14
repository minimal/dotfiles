#!/usr/bin/env bb
;; Unit tests for the pure functions in bin/nixpkgs-update-safe.bb.
;; End-to-end tests: scripts/nixpkgs-update-safe-test.sh.

(require '[clojure.test :refer [deftest is testing run-tests]]
         '[clojure.java.io :as io]
         '[clojure.string :as str])

(load-file (str (io/file (.getParentFile (.getParentFile (.getCanonicalFile (io/file *file*))))
                         "bin" "nixpkgs-update-safe.bb")))
(alias 'nus 'nixpkgs-update-safe)

(def drv-a "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-dotnet-sdk-8.0.drv")
(def drv-hm "/nix/store/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-home-manager-generation.drv")
(def drv-b "/nix/store/cccccccccccccccccccccccccccccccc-ripgrep-14.1.drv")

(deftest parse-uncached
  (testing "keeps only the will-be-built section, without the store prefix"
    (is (= ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-dotnet-sdk-8.0.drv"
            "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-home-manager-generation.drv"
            "cccccccccccccccccccccccccccccccc-ripgrep-14.1.drv"]
           (nus/parse-uncached
            (str/join "\n" ["warning: Git tree is dirty"
                            "these 3 derivations will be built:"
                            (str "  " drv-a) (str "  " drv-hm) (str "  " drv-b)
                            "these 2 paths will be fetched (1.0 MiB download):"
                            "  /nix/store/dddddddddddddddddddddddddddddddd-foo"])))))
  (testing "returns empty when all is cached"
    (is (= [] (nus/parse-uncached "these 2 paths will be fetched:\n  /nix/store/x-foo")))
    (is (= [] (nus/parse-uncached "")))))

(def listing
  (str "<Prefix>nixpkgs/nixpkgs-25.11pre100.aaaaaaaaaaaa/</Prefix>"
       "<Prefix>nixpkgs/nixpkgs-25.11pre300.cccccccccccc/</Prefix>"
       "<Prefix>nixpkgs/nixpkgs-25.11pre200.bbbbbbbbbbbb/</Prefix>"
       "<Prefix>nixpkgs/nixpkgs-25.11pre200.bbbbbbbbbbbb/</Prefix>"
       "<Prefix>nixpkgs/nixpkgs-25.11pre400.dddddddddddd/</Prefix>"))

(deftest older-releases
  (testing "returns releases older than the new rev, newest first, no duplicates"
    (is (= ["bbbbbbbbbbbb" "aaaaaaaaaaaa"]
           (nus/older-releases listing "cccccccccccc0123456789"))))
  (testing "returns nothing when the new rev is not a release"
    (is (empty? (nus/older-releases listing "ffffffffffff")))))

(deftest extract-update-date
  (let [lines ["• Updated input 'nixpkgs':"
               "    'github:NixOS/nixpkgs/aaa' (2024-01-15)"
               "• Updated input 'nixpkgs': 'github:x/aaa' (2024-01-15) → 'github:x/bbb' (2024-01-20)"
               "• Updated input 'home-manager': 'a' (2024-02-01) → 'b' (2024-02-03)"
               "• Updated input 'nixpkgs-lib': 'a' (2023-01-01) → 'b' (2023-01-02)"]]
    (is (= "2024-01-20" (nus/extract-update-date lines "nixpkgs")))
    (is (= "2024-02-03" (nus/extract-update-date lines "home-manager")))
    (is (nil? (nus/extract-update-date ["no dates"] "nixpkgs")))))

(deftest commit-message
  (is (= "nix: update nixpkgs & home-manager (2024-01-20, 2024-02-03)"
         (nus/commit-message [["nixpkgs" "2024-01-20"] ["home-manager" "2024-02-03"]])))
  (is (= "nix: update nixpkgs (2024-01-20)"
         (nus/commit-message [["nixpkgs" "2024-01-20"]])))
  (is (str/starts-with? (nus/commit-message []) "nix: update flake.lock (")))

(deftest format-date
  (is (= "2024-01-20" (nus/format-date "2024-01-20")))
  (is (= "2024-01-20" (nus/format-date "2024-01-20T10:00:00Z")))
  (is (= (str (java.time.LocalDate/now)) (nus/format-date nil))))

(deftest lock-reads
  (let [lock {"nodes" {"root" {"inputs" {"nixpkgs" "nixpkgs_2"}}
                       "nixpkgs" {"locked" {"rev" "wrong"}}
                       "nixpkgs_2" {"locked" {"rev" "right" "lastModified" 1705708800}}
                       "home-manager" {"locked" {"rev" "hm"}}}}]
    (testing "follows the root input indirection"
      (is (= "right" (nus/nixpkgs-rev lock)))
      (is (= "2024-01-20" (nus/nixpkgs-date lock))))
    (is (= "hm" (nus/hm-rev lock)))
    (testing "uses the nixpkgs node when the root input is a follows path"
      (is (= "wrong" (nus/nixpkgs-rev (assoc-in lock ["nodes" "root" "inputs" "nixpkgs"] ["a" "b"])))))))

(deftest json-log-lines
  (is (= ["evaluating" "line one" "line two" "post build" "not json"]
         (nus/json-log-lines
          (str/join "\n" ["{\"action\":\"msg\",\"level\":3,\"msg\":\"evaluating\"}"
                          "@nix {\"action\":\"result\",\"type\":101,\"fields\":[1,\"line one\\nline two\"]}"
                          "{\"action\":\"result\",\"type\":105,\"fields\":[1,2]}"
                          "{\"action\":\"result\",\"type\":107,\"fields\":[1,\"post build\"]}"
                          "{\"action\":\"start\",\"id\":1}"
                          "not json"]))))
  (testing "prefers raw_msg"
    (is (= ["raw"] (nus/json-log-lines "{\"action\":\"msg\",\"msg\":\"\\u001b[1mraw\",\"raw_msg\":\"raw\"}")))))

(deftest truncate
  (is (= "a" (nus/truncate "abc" 1)))
  (is (= "abc" (nus/truncate "abc" 80))))

(deftest describe-build
  (is (= "no compilation" (nus/describe-build [])))
  (is (= "1 derivation would compile" (nus/describe-build ["a"])))
  (is (= "3 derivations would compile" (nus/describe-build ["a" "b" "c"])))
  (is (= "cache check could not evaluate" (nus/describe-build ::nus/eval-failed))))

(deftest drv-compiles
  (testing "a source build compiles, and unknown attributes are a build"
    (is (true? (nus/drv-compiles?
                {"builder" "/nix/store/x-bash/bin/bash"
                 "env" {"__structuredAttrs" "" "src" "/nix/store/x-src"
                        "configurePhase" "configure"}})))
    (is (true? (nus/drv-compiles? {}))))

  (testing "generated files, wrappers, envs and fetches do not compile"
    (is (false? (nus/drv-compiles?
                 {"builder" "/nix/store/x-bash/bin/bash"
                  "structuredAttrs" {"text" "hello" "destination" "a.conf"}})))
    (is (false? (nus/drv-compiles?
                 {"builder" "/nix/store/x-bash/bin/bash"
                  "structuredAttrs" {"buildCommand" "ln -s $src $out"}})))
    (is (false? (nus/drv-compiles? {"builder" "builtin:buildenv"})))
    (is (false? (nus/drv-compiles?
                 {"builder" "/nix/store/x-bash/bin/bash"
                  "outputs" {"out" {"hash" "sha256-abc"}}})))
    (is (false? (nus/drv-compiles?
                 {"builder" "/nix/store/x-bash/bin/bash"
                  "env" {"phases" "installPhase"}})))
    (is (false? (nus/drv-compiles?
                 {"builder" "/nix/store/x-bash/bin/bash"
                  "structuredAttrs" {"dontUnpack" true "preferLocalBuild" true
                                     "buildPhase" "substitute $src $out"}}))))

  (testing "structured attributes in env.__json are read"
    (is (false? (nus/drv-compiles?
                 {"builder" "/nix/store/x-bash/bin/bash"
                  "env" {"__json" "{\"text\":\"hi\"}"}})))))

(deftest compiling-drvs
  (testing "an empty list needs no nix call"
    (is (= [] (nus/compiling-drvs []))))
  (testing "packages from a flake input are not cache misses"
    (is (= [] (nus/compiling-drvs
               ["p3bijqrscrhg8hlk2kirk3r7gsifsrg7-emacs-lsp-booster-0.2.1.drv"])))))

(deftest input-date
  (let [lock {"nodes" {"home-manager" {"locked" {"lastModified" 1705708800}}}}]
    (is (= "2024-01-20" (nus/input-date lock "home-manager")))
    (is (nil? (nus/input-date lock "missing")))))

(defn rev-of
  "Return a 40-character rev that starts with PREFIX."
  [prefix]
  (str prefix (apply str (repeat (- 40 (count prefix)) \0))))

(def dry-run-rev "oldnixpk")

(def dry-run-old-lock
  {"nodes" {"root" {"inputs" {"nixpkgs" "nixpkgs"}}
            "nixpkgs" {"locked" {"rev" (rev-of dry-run-rev) "lastModified" 1705708800}}
            "home-manager" {"locked" {"rev" "oldgen01"}}}})

(defn dry-run-lock
  "Return a lock with nixpkgs REV and given timestamps."
  [rev nixpkgs-ts hm-rev hm-ts]
  {"nodes" {"root" {"inputs" {"nixpkgs" "nixpkgs"}}
            "nixpkgs" {"locked" {"rev" rev "lastModified" nixpkgs-ts}}
            "home-manager" {"locked" {"rev" hm-rev "lastModified" hm-ts}}}})

(deftest dry-run-rows
  (testing "a fully cached latest nixpkgs is listed once"
    (let [chosen (dry-run-lock (rev-of "newnixpk") 1706572800 "newgen01" 1706659200)
          state (atom {:cache-checked {:rev (rev-of "newnixpk") :date "2024-01-30" :drvs []}})
          rows (nus/dry-run-rows state dry-run-old-lock chosen)]
      (is (= ["in use" "latest, no compilation"] (map :note rows)))
      (is (= ["2024-01-20" "2024-01-30"] (map :date rows)))
      (is (= ["oldnixpk" "newnixpk"] (map :rev rows)))))

  (testing "otherwise lists the latest and each rejected older release"
    (let [fallback (rev-of "cached")
          chosen (dry-run-lock fallback 1706486400 "oldgen01" nil)
          state (atom {:cache-checked {:rev (rev-of "newnixpk") :date "2024-01-30" :drvs ["a" "b"]}
                       :cache-tried [{:rev (subs (rev-of "oldrel") 0 12) :date "2024-01-30"
                                      :drvs ["a"]}
                                     {:rev (subs fallback 0 12) :date "2024-01-29" :drvs []}]})
          rows (nus/dry-run-rows state dry-run-old-lock chosen)]
      ;; The chosen release is left out; dry-run-summary reports it.
      (is (= ["in use"
              "latest, 2 derivations would compile"
              "older release, 1 derivation would compile"]
             (map :note rows)))
      (is (= ["oldnixpk" "newnixpk" "oldrel00"] (map :rev rows)))
      (is (= ["2024-01-20" "2024-01-30" "2024-01-30"] (map :date rows)))))

  (testing "an unchanged rev lists only the rev in use"
    (let [chosen (dry-run-lock (rev-of dry-run-rev) 1705708800 "newgen01" 1706659200)
          state (atom {:cache-checked {:rev (rev-of dry-run-rev) :date "2024-01-20"
                                      :drvs ["a" "b"]}})]
      (is (= ["in use"] (map :note (nus/dry-run-rows state dry-run-old-lock chosen)))))))

(deftest dry-run-summary
  (testing "reports the full rev, the date and the build state"
    (let [chosen (dry-run-lock (rev-of "newnixpk") 1706572800 "newgen01" 1706659200)
          state (atom {:cache-checked {:rev (rev-of "newnixpk") :date "2024-01-30" :drvs []}})
          lines (nus/dry-run-summary state dry-run-old-lock chosen {})]
      (is (= [(str "Would use nixpkgs " (rev-of "newnixpk") " (2024-01-30)"
                   ": no compilation")
              "Would use home-manager newgen01 (2024-01-31), was oldgen01"]
             (butlast lines)))
      (is (str/starts-with? (last lines) "Would commit: nix: update nixpkgs & home-manager ("))))

  (testing "a fallback release is reported as fully cached"
    (let [fallback (rev-of "cached")
          chosen (dry-run-lock fallback 1706486400 "oldgen01" nil)
          state (atom {:cache-checked {:rev (rev-of "newnixpk") :date "2024-01-30" :drvs ["a"]}})
          lines (nus/dry-run-summary state dry-run-old-lock chosen {})]
      (is (= (str "Would use nixpkgs " fallback " (2024-01-29): no compilation")
             (first lines)))
      (is (= 2 (count lines)))))

  (testing "compiling is reported when the latest rev is kept"
    (let [chosen (dry-run-lock (rev-of dry-run-rev) 1705708800 "oldgen01" nil)
          state (atom {:cache-checked {:rev (rev-of dry-run-rev) :date "2024-01-20"
                                      :drvs ["a" "b"]}})
          lines (nus/dry-run-summary state dry-run-old-lock chosen {})]
      (is (str/ends-with? (first lines) ": 2 derivations would compile"))))

  (testing "no fully cached release found: keep the current lock"
    (let [state (atom {:cache-checked {:rev (rev-of "newnixpk") :date "2024-01-30"
                                      :drvs ["a" "b"]}
                       :keep-current? true})]
      (is (= [(str "Would keep nixpkgs " (rev-of dry-run-rev) " (2024-01-20)"
                   ": no fully cached release newer than it")]
             (nus/dry-run-summary state dry-run-old-lock dry-run-old-lock {})))))

  (testing "an unreadable release index is reported as such"
    (let [state (atom {:cache-checked {:rev (rev-of "newnixpk") :date "2024-01-30"
                                      :drvs ["a"]}
                       :keep-current? true :index-failed? true})]
      (is (= [(str "Would keep nixpkgs " (rev-of dry-run-rev) " (2024-01-20)"
                   ": could not read the release index, so no older release was tried")]
             (nus/dry-run-summary state dry-run-old-lock dry-run-old-lock {})))))

  (testing "--nixpkgs-only leaves home-manager out"
    (let [chosen (dry-run-lock (rev-of "newnixpk") 1706572800 "newgen01" 1706659200)
          state (atom {:cache-checked {:rev (rev-of "newnixpk") :date "2024-01-30" :drvs []}})
          lines (nus/dry-run-summary state dry-run-old-lock chosen {:nixpkgs-only true})]
      (is (= 2 (count lines)))
      (is (not-any? #(str/includes? % "home-manager") (butlast lines))))))

(let [{:keys [fail error]} (run-tests)]
  (System/exit (if (zero? (+ fail error)) 0 1)))
