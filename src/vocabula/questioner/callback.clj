(ns vocabula.questioner.callback
  (:require [clojure.test :refer :all]
            [clojure.string :refer [join trim]]
            [vocabula.data :refer :all]
            [vocabula.question :refer :all])
  )


(declare make-callback-ask)


(deftest make-ask-test
  (let [q {:vocable {:left  [{:text "puella,ae"}]
                     :right [{:text "girl"}]
                     :rate 0}
           :side :left}
        cb (fn [q] 
             (is (= q ["puella,ae"]))
             "girl")
        ask (make-callback-ask cb)]
    (is (= (ask q)
           (assoc q :answer "girl")))))


(defn make-callback-ask
  "Creates a questioner forwarding questions the given callback."
  [cb]
  (fn [q]
    (let [side (:side q)
          words (-> q :vocable side)]
      (assoc q :answer (cb (map word->string words))))))
