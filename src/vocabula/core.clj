(ns vocabula.core
  (:require [vocabula.questioner.text :refer :all]
            [vocabula.question :refer :all]
            [vocabula.persist.vok :refer :all])
  (:gen-class))

(defn -main
  [& args]
  (let [filename (first args)
        vs (read-vok filename)]
    (write-vok filename
               (map (comp question->vocable ask vocable->question)
                    vs))))
