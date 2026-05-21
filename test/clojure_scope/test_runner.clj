(ns clojure-scope.test-runner
  (:require [clojure-scope.call-tree]
            [clojure-scope.core-test]
            [clojure-scope.line-region-test]
            [clojure-scope.move-test]
            [clojure-scope.dependency-graph]
            [clojure-scope.core]
            [clojure.test :as test]))

(defn -main [& _]
  (let [{:keys [fail error]} (test/run-tests 'clojure-scope.call-tree
                                             'clojure-scope.core-test
                                             'clojure-scope.line-region-test
                                             'clojure-scope.move-test
                                             'clojure-scope.core
                                             'clojure-scope.dependency-graph)]
    (when (pos? (+ fail error))
      (System/exit 1))))
