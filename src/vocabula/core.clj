(ns vocabula.core
  (:require [vocabula.question :refer :all]
            [vocabula.questioner.text :refer [ask]]
            [vocabula.persist.vok :refer :all]
            [vocabula.data :refer [filter-vocables]]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :refer [join]])
  (:gen-class))


(def cli-options
  [["-s" "--no-shuffle" "Do not shuffle vocables"
    :id :shuffle
    :default true]
   ["-l" "--limit" "Limit vocables to be asked"
    :id :limit
    :default 20
    :parse-fn #(Integer/parseInt %)]
   ["-w" "--worst" "Only ask worst vocables"
    :id :worst
    :default false]
   ["-h" "--help"]])


(defn usage
  [options-summary]
  (->> ["Simple command line vocable trainer."
        ""
        "Usage: vocabula [options] filename ..."
        ""
        "Options:"
        options-summary
        ""]
       (join "\n")))



(defn vocabula-main
  [retrieve store ask]
  (let [vs (shuffle (retrieve))]
    (store (map (comp question->vocable ask vocable->question)
                vs))))


(defn -main
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options)
      (do
        (println (usage summary))
        0)
      errors
      (do
        (println (join "\n" errors))
        1)
      :else
      (vocabula-main (fn []
                       (filter-vocables ((apply make-vok-reader arguments))
                                        options))
                     (apply make-vok-writer arguments)
                     ask))))
