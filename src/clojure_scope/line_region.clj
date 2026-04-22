(ns clojure-scope.line-region
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- read-file-lines [path]
  (let [file (io/file path)]
    (if (.exists file)
      (let [content (slurp file)]
        (if (empty? content)
          []
          (str/split-lines content)))
      [])))

(defn- write-file-lines! [path lines]
  (let [file (io/file path)]
    (.mkdirs (.getParentFile file))
    (spit file (str/join \newline lines))))

(defn insert-lines
  "inserts given lines to the target file starting from the target line"
  [target-file target-line lines]
  (let [target-lines (read-file-lines target-file)
        target-count (count target-lines)]

    (assert (>= target-line 1))
    (assert (<= target-line (inc target-count)))

    (write-file-lines! target-file
                       (vec (concat
                             (take (dec target-line) target-lines)
                             lines
                             (drop (dec target-line) target-lines))))

    {:target-line target-line
     :lines-inserted (count lines)}))

(defn delete-line-region [file first-line last-line]
  (let [lines (read-file-lines file)
        num-lines (count lines)]

    (assert (>= first-line 1))
    (assert (<= first-line num-lines))
    (assert (>= last-line first-line))
    (assert (<= last-line num-lines))

    (write-file-lines! file
                       (vec (concat (take (dec first-line) lines)
                                    (drop last-line lines))))

    {:first-line first-line
     :last-line last-line
     :lines-deleted (inc (- last-line first-line))}))

(defn copy-line-region [source-file target-file source-first-line source-last-line target-line]
  (let [lines (read-file-lines source-file)
        num-lines (count lines)]

    (assert (>= source-first-line 1))
    (assert (<= source-first-line num-lines))
    (assert (>= source-last-line source-first-line))
    (assert (<= source-last-line num-lines))

    (let [copied-lines (subvec (vec lines) (dec source-first-line) source-last-line)]
      (insert-lines target-file target-line copied-lines)
      {:source-first-line source-first-line
       :source-last-line source-last-line
       :target-line target-line
       :lines-copied (count copied-lines)})))
