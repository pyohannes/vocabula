(ns vocabula.persist.vok
  (:require [clojure.test :refer :all]
            [clojure.string :refer [trim starts-with?]]
            [vocabula.data :refer :all])
  )


(declare comment-or-empty-line?
         make-vok-reader make-vok-writer
         read-vok write-vok)


(deftest comment-or-empty-line?-test
  (is (comment-or-empty-line? ""))
  (is (comment-or-empty-line? "# comment"))
  (is (not (comment-or-empty-line? "a <> b")))
  (is (not (comment-or-empty-line? "a <> b # comment")))
  )

(defn- comment-or-empty-line?
  "Checks if a string from a vok file is a comment or an empty line."
  [line]
  (or (empty? line)
      (starts-with? line "#")))

(deftest read-vok-test
  (testing "Single file reads"
    (let [tempfile (java.io.File/createTempFile "test" ".vok")
          filename (.getAbsolutePath tempfile)
          vread (make-vok-reader filename)]
      (.deleteOnExit tempfile)
      (spit filename "puella,ae <> girl\n")
      (is (= (vread)
             [{:unit filename
               :rate  0
               :left  [{:text "puella,ae"}]
               :right [{:text "girl"}]}]))
      (spit filename "\n\npuella,ae <> girl\n\n")
      (is (= (vread)
             [{:unit filename
               :rate  0
               :left  [{:text "puella,ae"}]
               :right [{:text "girl"}]}]))
      (spit filename "puella,ae (f) <> girl\nbellus,a,um <4> pretty | beautiful\n")
      (is (= (vread)
             [{:unit filename
               :rate  0
               :left  [{:text "puella,ae" :desc "f"}]
               :right [{:text "girl"}]}
              {:unit filename
               :rate 4
               :left  [{:text "bellus,a,um"}]
               :right [{:text "pretty"} {:text "beautiful"}]}]))))
  (testing "Multiple file reads"
    (let [tempfile1 (java.io.File/createTempFile "test" ".vok")
          filename1 (.getAbsolutePath tempfile1)
          tempfile2 (java.io.File/createTempFile "test" ".vok")
          filename2 (.getAbsolutePath tempfile2)
          vread (make-vok-reader filename1 filename2)]
      (.deleteOnExit tempfile1)
      (.deleteOnExit tempfile2)
      (spit filename1 "puella,ae (f) <> girl\n")
      (spit filename2 "puer,pueri (m) <4> boy\n")
      (is (= (vread)
             [{:unit filename1
               :rate  0
               :left  [{:text "puella,ae" :desc "f"}]
               :right [{:text "girl"}]}
              {:unit filename2
               :rate 4
               :left  [{:text "puer,pueri" :desc "m"}]
               :right [{:text "boy"}]}]))))
  )

(defn make-vok-reader
 "Read vocabula entries from the files denoted by filenames. This returns
 a function that returns a list of vocabule entries."
  [& filenames]
  (fn []
    (reduce (fn [vs filename]
              (into vs (read-vok filename)))
            []
            filenames)))

(defn- read-vok
  "Read vocabula entries from a vok file. filename is the path to a vok file. 
  Returns a list of vocables."
  [filename]
  (let [add-unit-name (fn [v]
                        (assoc v :unit filename))]
    (with-open [rdr (clojure.java.io/reader filename)]
      (map
        (comp add-unit-name string->vocable)
        (filter (complement comment-or-empty-line?)
                (map trim (doall (line-seq rdr)))))))
  )

(deftest write-vok-test
  (let [tempfile (java.io.File/createTempFile "test" ".vok")
        filename (.getAbsolutePath tempfile)
        vwrite (make-vok-writer filename)]
    (.deleteOnExit tempfile)
    (testing "Single entry vok file"
      (vwrite [{:unit filename
                :rate  0
                :left  [{:text "puella,ae"}]
                :right [{:text "girl"}]}])
      (is (= (slurp filename)
             "puella,ae <> girl\n")))
    (testing "Two entry vok file"
      (spit filename "")
      (vwrite [{:unit filename
                :rate  0
                :left  [{:text "puella,ae" :desc "f"}]
                :right [{:text "girl"}]}
               {:unit filename
                :rate 4
                :left  [{:text "bellus,a,um"}]
                :right [{:text "pretty"} {:text "beautiful"}]}])
      (is (= (slurp filename)
             "puella,ae (f) <> girl\nbellus,a,um <4> pretty | beautiful\n")))
    (testing "Order preserved in vok file"
      (let [content "a <> b\nc <> d\ne <> f\ng <> h\n"
            vread (make-vok-reader filename)]
        (spit filename content)
        (vwrite (shuffle (vread)))
        (is (= (slurp filename) content))))
  ))

(defn make-vok-writer
  "Write vocabula entries into the file filename. This returns a function 
  that can be called with a list of vocabula entries."
  [& filenames]
  (fn [vs]
    (doseq [filename filenames]
      (write-vok filename
                 (filter #(= filename (:unit %)) vs)))))

(defn- write-vok
  "Write vocabula entries into a vok file. filename is the path to the vok file 
  to be written. vocables is a list of vocables."
  [filename vocables]
  (let [vocables (update-vocables (read-vok filename) vocables)]
    (with-open [wrt (clojure.java.io/writer filename)]
      (doseq [v vocables]
        (.write wrt (vocabula.data/vocable->string v))
        (.write wrt "\n"))))
  )


