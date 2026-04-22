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
      (let [content (slurp file)]
        (if (empty? content)
          []
          (str/split-lines content)))
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
      (is (thrown? AssertionError
                   (line-region/copy-line-region source-path target-path 0 1 1))))

    (testing "source last line must not exceed the source file"
      (is (thrown? AssertionError
                   (line-region/copy-line-region source-path target-path 1 3 1))))

    (testing "target line must be a valid insertion point"
      (is (thrown? AssertionError
                   (line-region/copy-line-region source-path target-path 1 1 0))))))

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
      (is (thrown? AssertionError
                   (line-region/insert-lines target-path 0 ["a"]))))

    (testing "target line must not exceed the last insertion point"
      (is (thrown? AssertionError
                   (line-region/insert-lines target-path 3 ["a"]))))))

(deftest delete-line-region-deletes-lines-in-range
  (let [dir (create-temp-dir)
        path (.getPath (io/file dir "file.txt"))]
    (write-lines! path ["a" "b" "c" "d"])

    (is (= {:first-line 2
            :last-line 3
            :lines-deleted 2}
           (line-region/delete-line-region path 2 3)))
    (is (= ["a" "d"] (read-lines path)))))

(deftest delete-line-region-deletes-first-line
  (let [dir (create-temp-dir)
        path (.getPath (io/file dir "file.txt"))]
    (write-lines! path ["a" "b" "c"])

    (is (= {:first-line 1
            :last-line 1
            :lines-deleted 1}
           (line-region/delete-line-region path 1 1)))
    (is (= ["b" "c"] (read-lines path)))))

(deftest delete-line-region-deletes-last-line
  (let [dir (create-temp-dir)
        path (.getPath (io/file dir "file.txt"))]
    (write-lines! path ["a" "b" "c"])

    (is (= {:first-line 3
            :last-line 3
            :lines-deleted 1}
           (line-region/delete-line-region path 3 3)))
    (is (= ["a" "b"] (read-lines path)))))

(deftest delete-line-region-deletes-all-lines
  (let [dir (create-temp-dir)
        path (.getPath (io/file dir "file.txt"))]
    (write-lines! path ["a" "b" "c"])

    (is (= {:first-line 1
            :last-line 3
            :lines-deleted 3}
           (line-region/delete-line-region path 1 3)))
    (is (= [] (read-lines path)))))

(deftest delete-line-region-validates-line-ranges
  (let [dir (create-temp-dir)
        path (.getPath (io/file dir "file.txt"))]
    (write-lines! path ["a" "b"])

    (testing "first line must be at least 1"
      (is (thrown? AssertionError
                   (line-region/delete-line-region path 0 1))))

    (testing "first line must not exceed last line"
      (is (thrown? AssertionError
                   (line-region/delete-line-region path 2 1))))

    (testing "first line must not exceed file length"
      (is (thrown? AssertionError
                   (line-region/delete-line-region path 3 4))))

    (testing "last line must not exceed file length"
      (is (thrown? AssertionError
                   (line-region/delete-line-region path 1 3))))))
