{:user {:plugins [ [lein-iclojure "1.2"]
                   [lein-exec "0.3.1"]
                   [lein-try "0.4.1"]
                   [lein-midje "3.1.3"]
                   [lein-ancient "0.5.4"]
                   [jonase/eastwood "0.1.0"]
                   [lein-difftest "2.0.0"]
                   [lein-dynalint "0.1.4"]]
        :dependencies [#_[clj-stacktrace "0.2.5"]
                       [spyscope "0.1.4"]
                       [org.clojure/tools.namespace "0.2.4"]
                       [io.aviso/pretty "0.1.8"]
                       [com.ambrosebs/dynalint "0.1.3"]
                       [compliment "0.0.3"]
                       [slamhound "1.5.1"]]
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
                        (constantly @#'io.aviso.repl/pretty-pst))
                      #_(alter-var-root #'clojure.repl/pst
                                      (constantly @#'io.aviso.repl/pretty-pst))

                     (require '[dynalint.lint :as dyn])
                     ]

        :jvm-opts [] ;; no opts for grench
        :repl-options {:nrepl-middleware [io.aviso.nrepl/pretty-middleware]}
        }


 :mirrors {"central" {:name "Ibiblio"
                     :url "http://mirrors.ibiblio.org/pub/mirrors/maven2"}
          #"clojars" {:name "Internal nexus"
                     :url "http://mvn.local/nexus/releases"
                     :repo-manager true}}}
