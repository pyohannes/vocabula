(ns vocabula.data
  (:require [clojure.test :refer :all]
            [clojure.string :refer :all])
  )

(declare word? word->string string->word
         vocable? vocable->string string->vocable)


(deftest word?-test
  (is (word? {:text "girl"}))
  (is (word? {:text "girl" :desc "f"}))
  (is (word? {:text "girl" :other "additional information"}))
  (is (not (word? {:desc "f"})))
  (is (not (word? {:text 0})))
  (is (not (word? {:text "girl" :desc 0})))
  (is (not (word? {})))
  )


(defn word?
  "Checks wether the argument w is a valid word."
  [w]
  (and (map? w)
       (contains? w :text)
       (string? (:text w))
       (if (contains? w :desc)
           (string? (:desc w))
           true)))


(deftest word->string-test
  (is (= (word->string {:text "puella,ae"})
         "puella,ae"))
  (is (= (word->string {:text "puella,ae" :desc "f"})
         "puella,ae (f)"))
  )


(defn word->string
  "Return a string representation of the word w."
  [w]
  (str (:text w)
       (if (contains? w :desc)
           (str " (" (:desc w) ")")
           "")))


(deftest string->word-test
  (doseq [s ["puella,ae"
             "puella,ae (f)"]]
    (is (= s
           (word->string (string->word s)))))
  )


(defn string->word
  "Transform a vok string to a word."
  [s]
  (let [m (re-matches #"^([^(]+)(?:\(([^)]*)\))? *$" s)
        text (trim (get m 1))
        desc (get m 2)]
    (conj {:text text }
          (if desc
              {:desc (trim desc)}
              {}))))


(deftest vocable?-test
  (is (vocable? {:left  [{:text "puella,ae"}]
                 :right [{:text "girl"}]
                 :rate  0}))
  (is (not (vocable? {:left  [{:text "puella,ae"}]
                      :right [{:text "girl"}]})))
  (is (not (vocable? {:left  [{:text "puella,ae"}]
                      :rate  0})))
  (is (not (vocable? {:right [{:text "puella,ae"}]
                      :rate  0})))
  (is (not (vocable? {:left  [{}]
                      :right [{:text "girl"}]
                      :rate  0})))
  (is (not (vocable? {:right [{}]
                      :left  [{:text "girl"}]
                      :rate  0})))
  )


(defn vocable?
  "Checks wether first argument v is a valid vocable."
  [v]
  (and (map? v)
       (every? #(contains? v %) [:right :left :rate])
       (number? (:rate v))
       (coll? (:right v))
       (coll? (:left v))
       (every? word? (:right v))
       (every? word? (:left v))))


(deftest vocable->string-test
  (is (= (vocable->string {:left  [{:text "puella,ae"}]
                           :right [{:text "girl"}]
                           :rate  0})
         "puella,ae <> girl"))
  (is (= (vocable->string {:left  [{:text "puella,ae"}]
                           :right [{:text "girl"}]
                           :rate  3})
         "puella,ae <3> girl"))
  (is (= (vocable->string {:left  [{:text "puella,ae" :desc "f"}]
                           :right [{:text "girl"}]
                           :rate  -4})
         "puella,ae (f) <-4> girl"))
  (is (= (vocable->string {:left  [{:text "bellus,a,um"}]
                           :right [{:text "nice"} {:text "beautiful"}]
                           :rate  0})
         "bellus,a,um <> nice | beautiful"))
  )


(defn vocable->string
  "Returns a string representation of the vocable v."
  [v]
  (str (clojure.string/join " | " (map word->string (:left v)))
       (if (= 0 (:rate v))
           " <> "
           (str " <" (:rate v) "> "))
       (clojure.string/join " | " (map word->string (:right v))))
  )


(deftest string->vocable-test
  (doseq [s ["puella,ae <> girl"
             "puella,ae <3> girl"
             "puella,ae (f) <-4> girl"
             "bellus,a,um <> nice | beautiful"]]
    (is (= s
           (vocable->string (string->vocable s)))))
  )


(defn string->vocable
  "Transform a vok string to a vocable"
  [s]
  (let [m (re-matches #"^([^<]+)<(?:(-?[0-9]*))?>(.+)$" s)
        left (get m 1)
        rate (trim (get m 2))
        right (get m 3)]
    {:left  (map string->word (split left #"\|"))
     :right (map string->word (split right  #"\|"))
     :rate  (if (empty? rate)
                0
                (Integer. rate))})
  )
