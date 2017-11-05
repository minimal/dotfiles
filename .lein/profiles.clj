{:user {:plugins [
                  [mvxcvi/whidbey "1.3.1"]
                  [lein-ancient "0.6.10"]
                  [cider/cider-nrepl "0.14.0"]
                  [refactor-nrepl "2.3.0"]
                  ;; [lein-try "0.4.3"]
                  ;; [io.aviso/pretty "0.1.23"]
                  [jonase/eastwood "0.2.3"]
                  [com.jakemccrary/lein-test-refresh "0.20.0"]
                  [venantius/yagni "0.1.4" :exclusions [org.clojure/clojure]]
                  [venantius/ultra "0.5.1" :exclusions [org.clojure/clojure]]
                  ;; [lein-kibit "0.1.2"]
                  [com.gfredericks/how-to-ns "0.1.5"]
                  ]

        :dependencies [[org.clojure/tools.nrepl "0.2.13"]
                       ;; [spyscope "0.1.5"]
                       [mvxcvi/puget "1.0.1"]
                       [org.clojure/tools.namespace "0.2.10"]
                       [io.aviso/pretty "0.1.33"]]
        :injections [(require #_'spyscope.core
                              'puget.printer)
                     ]
        :test-refresh {:notify-command ["terminal-notifier" "-title" "Tests" "-message"]
                       :changes-only true}
        }
 :mirrors {#"clojars" {:name "clojars mirror"
                       :url "https://clojars-mirror.tcrawley.org/repo/"}}}
