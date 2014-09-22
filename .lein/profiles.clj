{:user {:plugins [ #_[lein-iclojure "1.2"]
                   #_[lein-exec "0.3.3"]
                   [lein-try "0.4.3"]
                   [lein-midje "3.1.3"]
                   [lein-ancient "0.5.5"]
                   [cider/cider-nrepl "0.8.0-SNAPSHOT"]
                   [jonase/eastwood "0.1.4"]
                   [lein-kibit "0.0.8"]
                   [lein-difftest "2.0.0"]
                   [lein-dynalint "0.1.4"]
                   [lein-gorilla "0.3.3"]
                   ;; [mvxcvi/whidbey "0.3.2"] ;; doens't work with
                   [quickie "0.2.6"]]
        :dependencies [#_[clj-stacktrace "0.2.5"]
                       ;; [leiningen #=(leiningen.core.main/leiningen-version)]
                       ;; [im.chit/vinyasa "0.2.2"]
                       [spyscope "0.1.4"]
                       [io.aviso/pretty "0.1.12"]
                       [com.ambrosebs/dynalint "0.1.3"]
                       [org.clojure/tools.namespace "0.2.6"]
                       [compliment "0.1.3"]
                       [pjstadig/humane-test-output "0.6.0"]
                       [mvxcvi/puget "0.6.3"]
                       [slamhound "1.5.5"]]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :injections [#_(let [orig (ns-resolve (doto 'clojure.stacktrace require)
                                            'print-cause-trace)
                           new (ns-resolve (doto 'clj-stacktrace.repl require)
                                           'pst)]
                         (alter-var-root orig (constantly @new)))

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

                     (require '[dynalint.lint :as dyn])
                     (require '[clojure.tools.namespace.repl :as tnr :refer [refresh]])

                     (require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)

                     (require '[puget.printer :as puget])
                     ]


        :jvm-opts ["-Xmx1g" "-Djava.awt.headless=true"] ;; no opts for grench

        ;; :repl-options {:nrepl-middleware [io.aviso.nrepl/pretty-middleware]}
                     }


 :mirrors {"central" {:name "Ibiblio"
                     :url "http://mirrors.ibiblio.org/pub/mirrors/maven2"}
          #"clojars" {:name "Internal nexus"
                     :url "http://mvn.local/nexus/releases"
                     :repo-manager true}}}
