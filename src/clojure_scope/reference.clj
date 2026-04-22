(ns clojure-scope.reference
  (:require
   [clojure.java.io :as io]
   [rewrite-clj.zip :as zip]))

(set! *warn-on-reflection* true)

(defn replacement-symbol [new-namespace-alias new-name]
  (if new-namespace-alias
    (symbol (str new-namespace-alias) (str new-name))
    (symbol (str new-name))))

(defn position-before-or-equal? [left-line left-column right-line right-column]
  (or (< left-line right-line)
      (and (= left-line right-line)
           (<= left-column right-column))))

(defn position-within-range? [line column {:keys [row col end-row end-col]}]
  (and (position-before-or-equal? row col line column)
       (position-before-or-equal? line column end-row end-col)))

(defn symbol-at-position? [location line column]
  (and location
       (zip/sexpr-able? location)
       (symbol? (zip/sexpr location))
       (position-within-range? line column (meta (zip/node location)))))

(defn find-symbol-location [root-location line column]
  (loop [location root-location]
    (cond
      (nil? location)
      nil

      (zip/end? location)
      nil

      (symbol-at-position? location line column)
      location

      :else
      (recur (zip/next location)))))

(defn rewrite-reference [file-contents line column new-namespace-alias new-name]
  (let [root-location (zip/of-string file-contents)
        replacement (replacement-symbol new-namespace-alias new-name)]
    (if-let [symbol-location (find-symbol-location root-location line column)]
      (zip/root-string (zip/replace symbol-location replacement))
      (throw (ex-info "Could not find symbol reference at the given line and column."
                      {:line line
                       :column column})))))

(defn update-reference
  "Changes the var reference at LINE and COLUMN in FILE-NAME to use the given
  namespace alias and name. LINE and COLUMN are expected to be 1-based.

  If NEW-NAMESPACE-ALIAS is nil, the reference becomes unqualified.

  Returns the updated file contents."
  [file-name line column new-namespace-alias new-name]
  (let [^java.io.File file (io/file file-name)
        updated-file-contents (rewrite-reference (slurp file)
                                                 line
                                                 column
                                                 new-namespace-alias
                                                 new-name)]
    (spit file updated-file-contents)
    updated-file-contents))

(defn add-namespace-alias [file-name namespace alias])
