(ns clojure-scope.line-region
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- read-file-lines [path]
  (let [file (io/file path)]
    (if (.exists file)
      (str/split-lines (slurp file))
      [])))

(defn- write-file-lines! [path lines]
  (let [file (io/file path)]
    (.mkdirs (.getParentFile file))
    (spit file (str/join \newline lines))))

(defn copy-line-region [source-file target-file source-first-line source-last-line target-line]
  (let [lines (read-file-lines source-file)
        num-lines (count lines)]

    (assert (not (< source-first-line 1)))
    (assert (not (> source-first-line num-lines)))
    (assert (not (< source-last-line source-first-line)))
    (assert (not (> source-last-line num-lines)))

    (let [copied-lines (subvec (vec lines) (dec source-first-line) source-last-line)
          target-lines (read-file-lines target-file)
          target-count (count target-lines)]
      (assert (not (< target-line 1)))
      (assert (not (> target-line (inc target-count))))

      (write-file-lines! target-file
                         (vec (concat
                               (take (dec target-line) target-lines)
                               copied-lines
                               (drop (dec target-line) target-lines))))

      {:source-first-line source-first-line
       :source-last-line source-last-line
       :target-line target-line
       :lines-copied (count copied-lines)})))


(defn insert-lines
  "inserts given lines to the target file starting from the target line"
  [target-file target-line lines]
  (let [target-lines (read-file-lines target-file)
        target-count (count target-lines)]

    (assert (not (< target-line 1)))
    (assert (not (> target-line (inc target-count))))

    (write-file-lines! target-file
                       (vec (concat
                             (take (dec target-line) target-lines)
                             lines
                             (drop (dec target-line) target-lines))))

    {:target-line target-line
     :lines-inserted (count lines)}))
