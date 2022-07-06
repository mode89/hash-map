(defproject hashmap "0.1.0-SNAPSHOT"
  :dependencies [[criterium "0.4.6"]
                 [org.clojure/clojure "1.11.1"]
                 [org.clojure/test.check "1.1.1"]]
  :plugins [[lein-cloverage "1.2.4"]]
  :repl-options {:init-ns hashmap.core})
