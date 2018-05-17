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
  (let [tempfile (java.io.File/createTempFile "test" ".vok")
        filename (.getAbsolutePath tempfile)
        vread (make-vok-reader filename)]
    (.deleteOnExit tempfile)
    (spit filename "puella,ae <> girl\n")
    (is (= (vread)
           [{:rate  0
             :left  [{:text "puella,ae"}]
             :right [{:text "girl"}]}]))
    (spit filename "\n\npuella,ae <> girl\n\n")
    (is (= (vread)
           [{:rate  0
             :left  [{:text "puella,ae"}]
             :right [{:text "girl"}]}]))
    (spit filename "puella,ae (f) <> girl\nbellus,a,um <4> pretty | beautiful\n")
    (is (= (vread)
           [{:rate  0
             :left  [{:text "puella,ae" :desc "f"}]
             :right [{:text "girl"}]}
            {:rate 4
             :left  [{:text "bellus,a,um"}]
             :right [{:text "pretty"} {:text "beautiful"}]}])))
  )

(defn make-vok-reader
  [filename]
  (fn []
    (read-vok filename)))

(defn- read-vok
  "Read vocabula entries from a vok file. filename is the path to a vok file. 
  Returns a list of vocables."
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (map
      string->vocable
      (filter (complement comment-or-empty-line?)
              (map trim (doall (line-seq rdr))))))
  )

(deftest write-vok-test
  (let [tempfile (java.io.File/createTempFile "test" ".vok")
        filename (.getAbsolutePath tempfile)
        vwrite (make-vok-writer filename)]
    (.deleteOnExit tempfile)
    (vwrite [{:rate  0
              :left  [{:text "puella,ae"}]
              :right [{:text "girl"}]}])
    (is (= (slurp filename)
           "puella,ae <> girl\n"))
    (vwrite [{:rate  0
              :left  [{:text "puella,ae" :desc "f"}]
              :right [{:text "girl"}]}
             {:rate 4
              :left  [{:text "bellus,a,um"}]
              :right [{:text "pretty"} {:text "beautiful"}]}])
    (is (= (slurp filename)
           "puella,ae (f) <> girl\nbellus,a,um <4> pretty | beautiful\n"))
  ))

(defn make-vok-writer
  "Write vocabula entries into the file filename. This returns a function 
  that can be called with a list of vocabula entries."
  [filename]
  (fn [vs]
    (write-vok filename vs)))

(defn- write-vok
  "Write vocabula entries into a vok file. filename is the path to the vok file 
  to be written. vocables is a list of vocables."
  [filename vocables]
  (with-open [wrt (clojure.java.io/writer filename)]
    (doseq [v vocables]
      (.write wrt (vocabula.data/vocable->string v))
      (.write wrt "\n")))
  )


