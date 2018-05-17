(ns vocabula.data
  (:require [clojure.test :refer :all]
            [clojure.string :refer [trim join split]])
  )

(declare word? word->string string->word
         vocable? vocable->string string->vocable
         worst update-vocables filter-vocables)


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
  (str (join " | " (map word->string (:left v)))
       (if (= 0 (:rate v))
           " <> "
           (str " <" (:rate v) "> "))
       (join " | " (map word->string (:right v))))
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


(deftest worst-test
  (let [v1 (vocable->string "puella,ae <4> girl")
        v2 (vocable->string "magnus,a,um <-3> great")
        v3 (vocable->string "hodie <0> today")
        v4 (vocable->string "puer,i <-2> boy")]
    (let [w (worst 1 [v1 v2 v3 v4])]
      (is (.contains w v2)))
    (let [w (worst 2 [v1 v2 v3 v4])]
      (is (.contains w v2))
      (is (.contains w v4)))
    (let [w (worst 3 [v1 v2 v3 v4])]
      (is (.contains w v2))
      (is (.contains w v3))
      (is (.contains w v4)))
    (let [w (worst 4 [v1 v2 v3 v4])]
      (is (.contains w v1))
      (is (.contains w v2))
      (is (.contains w v3))
      (is (.contains w v4)))
  ))


(defn worst
  "Return the n vocables with the worst rate."
  [n vocables]
  (take n (sort-by :rate vocables)))


(deftest merge-vocables-test
  (testing "Simple updates"
    (let [v1 (string->vocable "puella,ae <4> girl")
          v2 (string->vocable "magnus,a,um <-3> great")
          v3 (string->vocable "hodie <0> today")
          vv1 (assoc v1 :rate -1)]
      (is (= (update-vocables [v1 v2 v3] [vv1 v3])
             [vv1 v2 v3]))))
  (testing "Append new vocables"
    (let [v1 (string->vocable "puella,ae <4> girl")
          v2 (string->vocable "magnus,a,um <-3> great")
          v3 (string->vocable "hodie <0> today")]
      (is (= (update-vocables [v1 v2] [v2 v3])
             [v1 v2 v3])))))
  

(defn update-vocables
  "Updates vocables in base with the changed ones in altered."
  [base altered]
  (let [reduce-vocable 
          (fn [v]
            {:left (:left v)
             :right (:right v)})
        vlist->set 
          (fn [l]
            [(reduce-vocable l) (:rate l)])
        aset (into {} (map vlist->set altered))
        bset (into {} (map  vlist->set base))
        get-rate
          (fn [key]
            (if (contains? aset key)
                (get aset key)
                (get bset key)))
        new-vocables (filter #(not (contains? bset (reduce-vocable %))) 
                             altered)]
    (into
      (reduce (fn [l item]
                (let [key (reduce-vocable item)]
                  (conj l
                        (assoc key :rate (get-rate key)))))
              []
              base)
      new-vocables)))
     

(deftest filter-vocables-test
  (let [v1 (string->vocable "a <> b")
        v2 (string->vocable "c <1> d")
        v3 (string->vocable "e <4> f")
        v4 (string->vocable "g <-3> h")]
    (testing "Order by worst vocables"
      (is (= (filter-vocables [v1 v2 v3 v4] {:worst true})
             [v4 v1 v2 v3])))
    (testing "Limit"
      (is (= (filter-vocables [v1 v2 v3 v4] {:limit 2})
             [v1 v2])))
    (testing "Limit and worst"
      (is (= (filter-vocables [v1 v2 v3 v4] {:worst true, :limit 2})
             [v4 v1])))))


(defn filter-vocables
  ""
  [vs attrs]
  (let [worst-apply 
         (fn [vs]
           (if (get attrs :worst)
               (sort-by :rate vs)
               vs))
        limit-apply
         (fn [vs]
           (if (get attrs :limit)
               (take (get attrs :limit) vs)
               vs))
        shuffle-apply
         (fn [vs]
           (if (get attrs :shuffle)
               (shuffle vs)
               vs))]
    (-> vs
        worst-apply
        limit-apply
        shuffle-apply)))
