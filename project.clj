(defproject vocabula "0.0.1-SNAPSHOT"
  :description "Simple command line vocabulary trainer"
  :url "https://github.com/pyohannes/vocabula"
  :license {:name "BSD 2-Clause License"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.cli "0.3.7"]]
  :main ^:skip-aot vocabula.core
  :target-path "target/%s"
  :test-paths ["src" "test"]
  :plugins [[lein-cloverage "1.0.10"]
            [lein-test-report-junit-xml "0.2.0"]]
  :profiles {:uberjar {:aot :all}})
