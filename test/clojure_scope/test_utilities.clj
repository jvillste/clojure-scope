(ns clojure-scope.test-utilities
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]))

(defmacro remove-indentation [string]
  (let [intendation (+ 20 (:column (meta &form)))
        rows (string/split string #"\n")]
    (string/join "\n"
                 (concat [(first rows)]
                         (map (fn [row]
                                (subs row (min intendation
                                               (count row))))
                              (rest rows))))))

(deftest test-remove-indentation
  (is (= "this is\n\nmultiline\nstring"
         (remove-indentation "this is

                              multiline
                              string")))

  (is (= "this is\n"
         (remove-indentation "this is
                              "))))
