(ns clojure-scope.line-region-test
  (:require [clojure-scope.line-region :as line-region]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- create-temp-dir []
  (.toFile
   (java.nio.file.Files/createTempDirectory
    "line-region-test"
    (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- write-lines! [path lines]
  (spit path (str/join "\n" lines)))

(defn- read-lines [path]
  (let [file (io/file path)]
    (if (.exists file)
      (str/split-lines (slurp file))
      [])))

(deftest copy-line-region-keeps-lines-in-source
  (let [dir (create-temp-dir)
        source-path (.getPath (io/file dir "source.txt"))
        target-path (.getPath (io/file dir "target.txt"))]
    (write-lines! source-path ["a" "b" "c" "d"])
    (write-lines! target-path ["x" "y"])

    (is (= {:source-first-line 2
            :source-last-line 3
            :target-line 2
            :lines-copied 2}
           (line-region/copy-line-region source-path target-path 2 3 2)))
    (is (= ["a" "b" "c" "d"] (read-lines source-path)))
    (is (= ["x" "b" "c" "y"] (read-lines target-path)))))

(deftest copy-line-region-creates-missing-target-file-and-parent-directories
  (let [dir (create-temp-dir)
        source-path (.getPath (io/file dir "source.txt"))
        target-file (io/file dir "nested" "target.txt")
        target-path (.getPath target-file)]
    (write-lines! source-path ["a" "b"])

    (line-region/copy-line-region source-path target-path 1 1 1)

    (is (.exists target-file))
    (is (= ["a"] (read-lines target-path)))
    (is (= ["a" "b"] (read-lines source-path)))))

(deftest copy-line-region-validates-line-ranges
  (let [dir (create-temp-dir)
        source-path (.getPath (io/file dir "source.txt"))
        target-path (.getPath (io/file dir "target.txt"))]
    (write-lines! source-path ["a" "b"])
    (write-lines! target-path ["x"])

    (testing "source first line must be within the source file"
      (let [ex (try
                 (line-region/copy-line-region source-path target-path 0 1 1)
                 nil
                 (catch clojure.lang.ExceptionInfo e
                   e))]
        (is (= :invalid-source-first-line (:type (ex-data ex))))))

    (testing "source last line must not exceed the source file"
      (let [ex (try
                 (line-region/copy-line-region source-path target-path 1 3 1)
                 nil
                 (catch clojure.lang.ExceptionInfo e
                   e))]
        (is (= :invalid-source-last-line (:type (ex-data ex))))))

    (testing "target line must be a valid insertion point"
      (let [ex (try
                 (line-region/copy-line-region source-path target-path 1 1 0)
                 nil
                 (catch clojure.lang.ExceptionInfo e
                   e))]
        (is (= :invalid-target-line (:type (ex-data ex))))))))

(deftest insert-lines-inserts-into-target-file
  (let [dir (create-temp-dir)
        target-path (.getPath (io/file dir "target.txt"))]
    (write-lines! target-path ["x" "y"])

    (is (= {:target-line 2
            :lines-inserted 2}
           (line-region/insert-lines target-path 2 ["a" "b"])))
    (is (= ["x" "a" "b" "y"]
           (read-lines target-path)))))

(deftest insert-lines-creates-missing-target-file-and-parent-directories
  (let [dir (create-temp-dir)
        target-file (io/file dir "nested" "target.txt")
        target-path (.getPath target-file)]
    (line-region/insert-lines target-path 1 ["a" "b"])

    (is (.exists target-file))
    (is (= ["a" "b"]
           (read-lines target-path)))))

(deftest insert-lines-validates-target-line
  (let [dir (create-temp-dir)
        target-path (.getPath (io/file dir "target.txt"))]
    (write-lines! target-path ["x"])

    (testing "target line must be at least 1"
      (let [ex (try
                 (line-region/insert-lines target-path 0 ["a"])
                 nil
                 (catch clojure.lang.ExceptionInfo e
                   e))]
        (is (= :invalid-target-line (:type (ex-data ex))))))

    (testing "target line must not exceed the last insertion point"
      (let [ex (try
                 (line-region/insert-lines target-path 3 ["a"])
                 nil
                 (catch clojure.lang.ExceptionInfo e
                   e))]
        (is (= :invalid-target-line (:type (ex-data ex))))))))
