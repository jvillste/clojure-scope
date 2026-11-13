(ns clojure-scope.spring-layout
  (:require
    [clojure.test :refer [deftest is]]))

(defn layout-dependency-graph
  "Uses simulated springs, to return coordinates for the nodes in a
  given dependency graph so that dependents and their dependants are
  given distance away from each other."
  [dependency-graph & [{:keys [node-distance]
                        :or {node-distance 10}}]]
  )

(deftest test-layout-dependency-graph
  (is (= {:a1 {:x 1 :y 1}
          :b1 {:x 1 :y 1}
          :c1 {:x 1 :y 1}
          :a2 {:x 1 :y 1}
          :b2 {:x 1 :y 1}
          :c2 {:x 1 :y 1}}

         (layout-dependency-graph [{:dependent :a1
                                    :dependency :c2}
                                   {:dependent :b1
                                    :dependency :a2}
                                   {:dependent :c1
                                    :dependency :b2}]))))
