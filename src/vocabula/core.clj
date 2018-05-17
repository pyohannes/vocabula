(ns vocabula.core
  (:require [vocabula.question :refer :all]
            [vocabula.questioner.text :refer [ask]]
            [vocabula.persist.vok :refer :all])
  (:gen-class))


(defn vocabula-main
  [retrieve store ask]
  (let [vs (retrieve)]
    (store (map (comp question->vocable ask vocable->question)
                vs))))


(defn -main
  [& args]
  (vocabula-main (make-vok-reader (first args))
                 (make-vok-writer (first args))
                 ask))
