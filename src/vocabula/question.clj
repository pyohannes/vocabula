(ns vocabula.question
  (:require [clojure.test :refer :all]
            [clojure.string :refer [trim starts-with?]]
            [vocabula.data :refer :all])
  )


(declare question? correctly-answered? vocable->question question->vocable)


(deftest question?-test
  (is (question? {:vocable {:left  [{:text "puella,ae"}]
                            :right [{:text "girl"}]
                            :rate 0}
                  :side :left}))
  (is (question? {:vocable {:left  [{:text "puella,ae"}]
                            :right [{:text "girl"}]
                            :rate 0}
                  :side :left
                  :answer "girl"}))
  (is (not (question? {:vocable "v"
                       :side :left})))
  (is (not (question? {:vocable {:left  [{:text "puella,ae"}]
                                 :right [{:text "girl"}]
                                 :rate 0}
                       :answer "girl"})))
  )


(defn question?
  "Check wether the argument q is a question."
  [q]
  (and (contains? q :vocable)
       (vocable? (:vocable q))
       (contains? q :side)
       (or (= (:side q) :left)
           (= (:side q) :right))
       (if (contains? q :answer)
           (string? (:answer q))
           true)))


(deftest correctly-answered?-test
  (is (correctly-answered? {:vocable {:left  [{:text "bellus,a,um"}]
                                      :right [{:text "nice"} {:text "pretty"}]
                                      :rate 0}
                            :side :left
                            :answer "nice"}))
  (is (correctly-answered? {:vocable {:left  [{:text "bellus,a,um"}]
                                      :right [{:text "nice"} {:text "pretty"}]
                                      :rate 0}
                            :side :left
                            :answer "pretty"}))
  (is (not (correctly-answered? 
             {:vocable {:left  [{:text "bellus,a,um"}]
                        :right [{:text "nice"} {:text "pretty"}]
                        :rate 0}
              :side :right
              :answer "pretty"})))
  )


(defn correctly-answered?
  "Returns true if the answer stored in the question is correct."
  [q]
  (let [side (if (= :left (:side q))
                 :right
                 :left)
        words (-> q :vocable side)]
    (.contains (map :text words)
               (:answer q))))


(deftest vocable->question-test
  (let [v {:left  [{:text "puella,ae"}]
           :right [{:text "girl"}]
           :rate  0}
        q (vocable->question v)]
    (is (= v (:vocable q)))
    (is (question? q))
    (is (contains? q :side))
    (is (not (contains? q :answer)))))


(defn vocable->question
  "Creates a question from a vocable."
  [v]
  {:vocable v
   :side (rand-nth [:left :right])})


(deftest question->vocable-test
  (let [v {:left  [{:text "puella,ae"}]
           :right [{:text "girl"}]
           :rate  0}
        q (vocable->question v)
        wrong (assoc q :answer "wrong")
        right (assoc (assoc q :side :left) :answer "girl")]
    (is (= (question->vocable wrong)
           (assoc v :rate -5)))
    (is (= (question->vocable right)
           (assoc v :rate 1))))
  )


(defn question->vocable
  "Transform a question in a vocable. Information about right or wrong answers
  is considered in the rate of the vocable."
  [q]
  (let [v (:vocable q)
        rate (:rate v)]
    (assoc v 
           :rate
           (if (correctly-answered? q)
               (+ rate 1)
               (- rate 5)))))

