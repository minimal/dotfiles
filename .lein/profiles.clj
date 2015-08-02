{:user {:plugins [[mvxcvi/whidbey "1.0.0" :exclusions [mvxcvi/puget]]
                  [lein-try "0.4.3"]
                  [lein-ancient "0.6.7"]
                  [cider/cider-nrepl "0.9.0"]
                  [refactor-nrepl "1.0.5"]
                  [jonase/eastwood "0.2.1"]
                  [lein-kibit "0.1.2"]]

        :dependencies [[org.clojure/tools.nrepl "0.2.10"]
                       [spyscope "0.1.5"]
                       [org.clojure/tools.namespace "0.2.10"]
                       [mvxcvi/puget "0.8.1"]]
        :injections [(require 'spyscope.core 'puget.printer)
                     #_(try
                         (require 'cider.nrepl.middleware.pprint)
                         (alter-var-root #'cider.nrepl.middleware.pprint/pprint-eval
                           (constantly (fn [form] (let [res (eval form)]
                                                   (puget.printer/pprint res) res))))
                         (catch Exception e
                           (println "Error loading nrepl mw" e)))]}

 #_{:plugins [#_[lein-iclojure "1.2"]
              #_[lein-exec "0.3.3"]
              [lein-try "0.4.3"]
              [lein-ancient "0.6.5"]
              [cider/cider-nrepl "0.9.0-SNAPSHOT"]
              [refactor-nrepl "0.3.0-SNAPSHOT"]
              [jonase/eastwood "0.2.1"]
              [lein-kibit "0.0.8"]
              [lein-dynalint "0.1.4"]

              ;; [mvxcvi/whidbey "0.5.1" :exclusions [mvxcvi/puget]]
              ;; [mvxcvi/whidbey "0.6.0"]
              [quickie "0.3.6"]]
    :dependencies [#_[clj-stacktrace "0.2.5"]
                   [org.clojure/tools.nrepl "0.2.8" #_"0.2.7"]
                   [spyscope "0.1.5"]
                   [mvxcvi/puget "0.8.0"]
                   [io.aviso/pretty "0.1.17"]
                   [com.ambrosebs/dynalint "0.1.3"]
                   [org.clojure/tools.namespace "0.2.10"]
                   [flare "0.2.9-SNAPSHOT"]
                   [pjstadig/humane-test-output "0.6.1-SNAPSHOT"]

                   ;; [pjstadig/humane-test-output "0.6.0"]
                   ;; [mvxcvi/puget "0.6.4"]
                   [acyclic/squiggly-clojure "0.1.2-SNAPSHOT"]
                   [slamhound "1.5.5"]]
    :aliases {"slamhound" ["run" "-m" "slam.hound"]}
    :injections [

                 ;; from
                 ;; http://z.caudate.me/give-your-clojure-workflow-more-flow/
                 (require 'spyscope.core)

                 (require 'io.aviso.repl
                          'clojure.repl
                          'clojure.main)
                 #_(alter-var-root #'clojure.main/repl-caught
                     (constantly @#'io.aviso.repl/pretty-repl-caught))
                 #_(alter-var-root #'clojure.repl/pst
                     (constantly @#'io.aviso.repl/pretty-pst))
                 ;; (require 'puget.printer)
                 #_(alter-var-root #'clojure.pprint/pprint
                     (constantly #(puget.printer/with-options
                                    {:color-scheme {:keyword [:red]
                                                    :nil [:blue :bold]
                                                    :delimiter [:blue]}}
                                    (puget.printer/pprint %))))

                 #_(require '[dynalint.lint :as dyn])
                 (require '[clojure.tools.namespace.repl :as tnr :refer [refresh]])

                 (require 'pjstadig.humane-test-output)
                 (pjstadig.humane-test-output/activate!)
                 #_(alter-var-root #'cider.nrepl.middleware.pprint/pprint-eval
                     (constantly (fn [form] (let [res (eval form)] (puget.printer/pprint res) res))))
                 ;; (require 'flare.midje)
                 ;; (flare.midje/install!)
                 ;; (require '[puget.printer :as puget :refer (cprint)])
                 ]


    ;;        :jvm-opts ["-Xmx1g" "-Djava.awt.headless=true"] ;; no opts for grench

    ;; :repl-options {:nrepl-middleware [io.aviso.nrepl/pretty-middleware]}
    }


 :mirrors {"central" {:name "Ibiblio"
                      :url "http://mirrors.ibiblio.org/pub/mirrors/maven2"}
           #"clojars" {:name "Internal nexus"
                       :url "http://mvn.local/nexus/releases"
                       :repo-manager true}} :cuttle {:plugins [[lein-pprint "1.1.2"]]}}
