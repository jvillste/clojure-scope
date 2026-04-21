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

(defn copy-line-region
  [source-file target-file source-first-line source-last-line target-line]
  (let [lines (read-file-lines source-file)
        num-lines (count lines)]

    (when (< source-first-line 1)
      (throw (ex-info (format "%d is out of range (1-%d)" source-first-line num-lines)
                      {:type :invalid-source-first-line
                       :source-first-line source-first-line
                       :max num-lines})))
    (when (> source-first-line num-lines)
      (throw (ex-info (format "%d is out of range (1-%d)" source-first-line num-lines)
                      {:type :invalid-source-first-line
                       :source-first-line source-first-line
                       :max num-lines})))
    (when (< source-last-line source-first-line)
      (throw (ex-info (format "%d is out of range (%d-%d)" source-last-line source-first-line num-lines)
                      {:type :invalid-source-last-line
                       :source-last-line source-last-line
                       :min source-first-line
                       :max num-lines})))
    (when (> source-last-line num-lines)
      (throw (ex-info (format "%d is out of range (%d-%d)" source-last-line source-first-line num-lines)
                      {:type :invalid-source-last-line
                       :source-last-line source-last-line
                       :min source-first-line
                       :max num-lines})))

    (let [copied-lines (subvec (vec lines) (dec source-first-line) source-last-line)
          target-lines (read-file-lines target-file)
          target-count (count target-lines)]
      (when (< target-line 1)
        (throw (ex-info (format "%d is out of range (1-%d)" target-line (inc target-count))
                        {:type :invalid-target-line
                         :target-line target-line
                         :max (inc target-count)})))
      (when (> target-line (inc target-count))
        (throw (ex-info (format "%d is out of range (1-%d)" target-line (inc target-count))
                        {:type :invalid-target-line
                         :target-line target-line
                         :max (inc target-count)})))

      (write-file-lines! target-file
                         (vec (concat
                               (take (dec target-line) target-lines)
                               copied-lines
                               (drop (dec target-line) target-lines))))

      {:source-first-line source-first-line
       :source-last-line source-last-line
       :target-line target-line
       :lines-copied (count copied-lines)})))
