{:user {:plugins [
                  ;; [mvxcvi/whidbey "1.3.2"]
                  [lein-ancient "0.6.15"]
                  [cider/cider-nrepl "0.16.0"]
                  [refactor-nrepl "2.3.1"]
                  ;; [lein-try "0.4.3"]
                  ;; [io.aviso/pretty "0.1.23"]
                  [jonase/eastwood "0.2.5"]
                  [com.jakemccrary/lein-test-refresh "0.22.0"]
                  [venantius/yagni "0.1.4" :exclusions [org.clojure/clojure]]
                  ;; [venantius/ultra "0.5.2" :exclusions [org.clojure/clojure]]
                  ;; [lein-kibit "0.1.2"]
                  [com.gfredericks/how-to-ns "0.1.8"]
                  ]

        :dependencies [[org.clojure/tools.nrepl "0.2.13"]
                       ;; [spyscope "0.1.6"]
                       [mvxcvi/puget "1.0.2"]
                       [org.clojure/tools.namespace "0.2.10"]
                       ;; [io.aviso/pretty "0.1.34"]
                       ]
        :injections [(require ;;'spyscope.core
                      'puget.printer)
                     ]
        :test-refresh {:notify-command ["terminal-notifier" "-title" "Tests" "-message"]
                       :changes-only true}
        }
 :mirrors {#"clojars" {:name "clojars mirror"
                       :url "https://clojars-mirror.tcrawley.org/repo/"}}}
