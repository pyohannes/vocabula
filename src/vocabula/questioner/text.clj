(ns vocabula.questioner.text
  (:require [clojure.test :refer :all]
            [clojure.string :refer [join trim]]
            [vocabula.data :refer :all]
            [vocabula.question :refer :all])
  )


(declare ask)


(defn- string->stream
  [s]
  (-> s
      java.io.StringReader.
      java.io.BufferedReader.))


(deftest ask-test
  (let [q {:vocable {:left  [{:text "puella,ae"}]
                     :right [{:text "girl"}]
                     :rate 0}
           :side :left}]
    (binding [*in* (string->stream "girl\n")
              *out* (java.io.StringWriter.)]
      (is (= (ask q)
             (assoc q :answer "girl")))
      (is (= (str *out*)
             "\npuella,ae\nCorrect.\n")))
    (binding [*in* (string->stream "boy\n")
              *out* (java.io.StringWriter.)]
      (is (= (ask q)
             (assoc q :answer "boy")))
      (is (= (str *out*)
             "\npuella,ae\nWRONG:  girl\n")))))


(defn ask
  "Ask a question and store the given answer."
  [q]
  (let [side (:side q)
        words (-> q :vocable side)]
    (println "")
    (println (join " | " (map word->string words)))
    (let [q (assoc q :answer (trim (read-line)))]
      (if (correctly-answered? q)
          (println "Correct.")
          (let [other-side (if (= side :left)
                               :right
                               :left)
                answers (-> q :vocable other-side)]
            (println "WRONG: " (join " | " (map word->string answers)))))
      q)))
