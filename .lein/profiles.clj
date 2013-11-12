{:user {:plugins [ [lein-iclojure "1.2"]
                   [lein-exec "0.3.0"]
                   [lein-try "0.3.0"]
                   [lein-midje "3.1.1"]
                   [lein-difftest "2.0.0"]]
        :dependencies [[clj-stacktrace "0.2.5"]]
        :injections [(let [orig (ns-resolve (doto 'clojure.stacktrace require)
                                            'print-cause-trace)
                           new (ns-resolve (doto 'clj-stacktrace.repl require)
                                           'pst)]
                       (alter-var-root orig (constantly @new)))]
        :jvm-opts [] ;; no opts for grench
        }

 :mirrors {"central" {:name "Ibiblio"
                     :url "http://mirrors.ibiblio.org/pub/mirrors/maven2"}
          #"clojars" {:name "Internal nexus"
                     :url "http://mvn.local/nexus/releases"
                     :repo-manager true}}}
