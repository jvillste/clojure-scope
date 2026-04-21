(ns clojure-scope.test-runner
  (:require [clojure-scope.core-test]
            [clojure-scope.line-region-test]
            [clojure-scope.core]
            [clojure.test :as test]))

(defn -main [& _]
  (let [{:keys [fail error]} (test/run-tests 'clojure-scope.core-test
                                             'clojure-scope.line-region-test
                                             'clojure-scope.core)]
    (when (pos? (+ fail error))
      (System/exit 1))))
