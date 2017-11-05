(set-env! :dependencies '[[boot-deps "0.1.8"]
                          ])
(swap! boot.repl/*default-dependencies* conj
       ;; '[mvxcvi/whidbey "1.3.1"]
       '[refactor-nrepl "2.3.1"]
       '[cider/cider-nrepl "0.15.1"])

(swap! boot.repl/*default-middleware* conj
       ;; 'clojure.tools.nrepl.middleware.render-values/render-values
       'refactor-nrepl.middleware/wrap-refactor
       'cider.nrepl/cider-middleware
       )

(require '[boot-deps :refer [ancient]])
