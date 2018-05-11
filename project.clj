(defproject vocabula "0.0.1-SNAPSHOT"
  :description "Simple command line vocabulary trainer"
  :url "https://github.com/pyohannes/vocabula"
  :license {:name "BSD 2-Clause License"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot vocabula.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
