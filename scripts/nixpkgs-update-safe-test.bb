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
  (testing "keeps only the will-be-built section, without local derivations"
    (is (= ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-dotnet-sdk-8.0.drv"
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

(let [{:keys [fail error]} (run-tests)]
  (System/exit (if (zero? (+ fail error)) 0 1)))
