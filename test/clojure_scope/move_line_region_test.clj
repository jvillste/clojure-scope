(ns clojure-scope.move-line-region-test
  (:require [clojure-scope.move-line-region :as move-line-region]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- create-temp-dir []
  (.toFile
   (java.nio.file.Files/createTempDirectory
    "move-line-region-test"
    (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- write-lines! [path lines]
  (spit path (str/join "\n" lines)))

(defn- read-lines [path]
  (let [file (io/file path)]
    (if (.exists file)
      (str/split-lines (slurp file))
      [])))

(deftest move-line-region-removes-lines-from-source-by-default
  (let [dir (create-temp-dir)
        source-path (.getPath (io/file dir "source.txt"))
        target-path (.getPath (io/file dir "target.txt"))]
    (write-lines! source-path ["a" "b" "c" "d"])
    (write-lines! target-path ["x" "y"])

    (is (= {:source-first-line 2
            :source-last-line 3
            :target-line 2
            :lines-moved 2}
           (move-line-region/move-line-region source-path target-path 2 3 2)))
    (is (= ["a" "d"] (read-lines source-path)))
    (is (= ["x" "b" "c" "y"] (read-lines target-path)))))

(deftest move-line-region-can-keep-lines-in-source
  (let [dir (create-temp-dir)
        source-path (.getPath (io/file dir "source.txt"))
        target-path (.getPath (io/file dir "target.txt"))]
    (write-lines! source-path ["a" "b" "c"])
    (write-lines! target-path ["x"])

    (is (= {:source-first-line 1
            :source-last-line 2
            :target-line 2
            :lines-moved 2}
           (move-line-region/move-line-region source-path target-path 1 2 2 false)))
    (is (= ["a" "b" "c"] (read-lines source-path)))
    (is (= ["x" "a" "b"] (read-lines target-path)))))

(deftest move-line-region-creates-missing-target-file-and-parent-directories
  (let [dir (create-temp-dir)
        source-path (.getPath (io/file dir "source.txt"))
        target-file (io/file dir "nested" "target.txt")
        target-path (.getPath target-file)]
    (write-lines! source-path ["a" "b"])

    (move-line-region/move-line-region source-path target-path 1 1 1)

    (is (.exists target-file))
    (is (= ["a"] (read-lines target-path)))
    (is (= ["b"] (read-lines source-path)))))

(deftest move-line-region-validates-line-ranges
  (let [dir (create-temp-dir)
        source-path (.getPath (io/file dir "source.txt"))
        target-path (.getPath (io/file dir "target.txt"))]
    (write-lines! source-path ["a" "b"])
    (write-lines! target-path ["x"])

    (testing "source first line must be within the source file"
      (let [ex (try
                 (move-line-region/move-line-region source-path target-path 0 1 1)
                 nil
                 (catch clojure.lang.ExceptionInfo e
                   e))]
        (is (= :invalid-source-first-line (:type (ex-data ex))))))

    (testing "source last line must not exceed the source file"
      (let [ex (try
                 (move-line-region/move-line-region source-path target-path 1 3 1)
                 nil
                 (catch clojure.lang.ExceptionInfo e
                   e))]
        (is (= :invalid-source-last-line (:type (ex-data ex))))))

    (testing "target line must be a valid insertion point"
      (let [ex (try
                 (move-line-region/move-line-region source-path target-path 1 1 0)
                 nil
                 (catch clojure.lang.ExceptionInfo e
                   e))]
        (is (= :invalid-target-line (:type (ex-data ex))))))))
