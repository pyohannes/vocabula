(ns vocabula.vok_test
  (:require [clojure.test :refer :all]
            [vocabula.core :refer [vocabula-main]]
            [vocabula.data :refer [filter-vocables]]
            [vocabula.persist.vok :refer [make-vok-reader make-vok-writer]]
            [vocabula.questioner.callback :refer [make-callback-ask]]))


(defn- write-into-tempfile
  [text]
  (let [tempfile (java.io.File/createTempFile "test" ".tmp")
        filename (.getAbsolutePath tempfile)]
    (.deleteOnExit tempfile)
    (spit filename text)
    filename))


(defn- answer-correctly
  [answers]
  (let [answers (reduce (fn [mp [k v]]
                          (assoc mp v k))
                        answers
                        (seq answers))]
    (fn [qs]
      (get answers (first qs)))))


(deftest cmd-vok-one-entry-test-right
  (let [filename (write-into-tempfile "puella,ae <> girl\n")
        cb (answer-correctly { "puella,ae" "girl" })]
    (vocabula-main (make-vok-reader filename)
                   (make-vok-writer filename)
                   (make-callback-ask cb))
    (is (= (slurp filename)
           "puella,ae <1> girl\n"))))


(deftest cmd-vok-one-entry-test-wrong
  (let [filename (write-into-tempfile "puella,ae <> girl\n")
        cb (fn [q] "boy")]
    (vocabula-main (make-vok-reader filename)
                   (make-vok-writer filename)
                   (make-callback-ask cb))
    (is (= (slurp filename)
           "puella,ae <-5> girl\n"))))


(deftest cmd-vok-multiple-entry-test-right
  (let [filename (write-into-tempfile "puella,ae <> girl\npuer,pueri <> boy\n")
        cb (answer-correctly { "puella,ae" "girl"
                               "puer,pueri" "boy" })]
    (vocabula-main (make-vok-reader filename)
                   (make-vok-writer filename)
                   (make-callback-ask cb))
    (is (= (slurp filename)
           "puella,ae <1> girl\npuer,pueri <1> boy\n"))))


(deftest cmd-vok-multiple-files
  (let [filename1 (write-into-tempfile "puella,ae <> girl\n")
        filename2 (write-into-tempfile "puer,pueri <> boy\n")
        cb (answer-correctly { "puella,ae" "girl"
                               "puer,pueri" "boy" })]
    (vocabula-main (make-vok-reader filename1 filename2)
                   (make-vok-writer filename1 filename2)
                   (make-callback-ask cb))
    (is (= (slurp filename1) "puella,ae <1> girl\n"))
    (is (= (slurp filename2) "puer,pueri <1> boy\n"))))


(deftest cmd-vok-multiple-entry-ordered-rewrite
  (let [filename (write-into-tempfile "a <> b\nc <> d\ne <> f\ng <> h\n")
        cb (answer-correctly { "a" "b", "c" "d", "e" "f", "g" "h" })]
    (vocabula-main (make-vok-reader filename)
                   (make-vok-writer filename)
                   (make-callback-ask cb))
    (is (= (slurp filename)
           "a <1> b\nc <1> d\ne <1> f\ng <1> h\n"))))


(deftest cmd-vok-multiple-entry-filters
  (let [filename (write-into-tempfile "a <3> b\nc <-4> d\ne <1> f\ng <> h\n")
        cb (answer-correctly { "a" "b", "c" "d", "e" "f", "g" "h" })]
    (vocabula-main (fn []
                     (filter-vocables ((make-vok-reader filename))
                                      {:shuffle true
                                       :worst true
                                       :limit 3}))
                   (make-vok-writer filename)
                   (make-callback-ask cb))
    (is (= (slurp filename)
           "a <3> b\nc <-3> d\ne <2> f\ng <1> h\n"))))
