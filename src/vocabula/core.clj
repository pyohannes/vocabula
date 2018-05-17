(ns vocabula.core
  (:require [vocabula.question :refer :all]
            [vocabula.persist.vok :refer :all])
  (:gen-class))


(defn vocabula-main
  [retrieve store ask]
  (let [vs (retrieve)]
    (store (map (comp question->vocable ask vocable->question)
                vs))))


(defn -main
  [& args]
  (vocabula-main (fn [] (read-vok (first args)))
                 (fn [vs] (write-vok (first args) vs))
                 vocabula.questioner.text/ask))
