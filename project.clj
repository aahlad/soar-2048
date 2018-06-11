(defproject soar-2048 "1.0.0"
  :description "FIXME: write description"
  :url "https://github.com/fli/soar-2048"
  :license {:name "MIT"
            :url "https://mit-license.org"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [edu.umich.soar/sml "9.5.1"]]
  :repositories {"project" "file:local-mvn-repo"}
  :main ^:skip-aot soar-2048.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
