(ns clojure-scope.dependency-garph
  (:require
   [clojure-scope.core :as clojure-scope]
   [clojure.test :refer [deftest is testing]]))


(defn dependency-chain-depth
  "returns the length of the longest dependency path starting from the given var"
  [dependency-graph var]
  (let [dependencies-by-var (clojure-scope/dependencies-by-var dependency-graph)]
    (letfn [(length-from [current-var visited-vars]
              (let [dependencies (remove visited-vars
                                         (get dependencies-by-var current-var))]
                (if (empty? dependencies)
                  0
                  (apply max
                         (map (fn [dependency-var]
                                (inc (length-from dependency-var
                                                  (conj visited-vars current-var))))
                              dependencies)))))]
      (length-from var #{var}))))

(deftest test-dependency-chain-depth
  (testing "returns the longest chain length in edges"
    (is (= 3
           (dependency-chain-depth [{:dependent ["ns" "a"]
                                     :dependency ["ns" "b"]}
                                    {:dependent ["ns" "b"]
                                     :dependency ["ns" "c"]}
                                    {:dependent ["ns" "b"]
                                     :dependency ["ns" "d"]}
                                    {:dependent ["ns" "d"]
                                     :dependency ["ns" "e"]}]
                                   ["ns" "a"]))))

  (testing "returns zero for vars with no dependencies"
    (is (= 0
           (dependency-chain-depth []
                                   ["ns" "a"]))))

  (testing "ignores cycles when computing the longest simple path"
    (is (= 2
           (dependency-chain-depth [{:dependent ["ns" "a"]
                                     :dependency ["ns" "b"]}
                                    {:dependent ["ns" "b"]
                                     :dependency ["ns" "a"]}
                                    {:dependent ["ns" "b"]
                                     :dependency ["ns" "c"]}]
                                   ["ns" "a"])))))

(defn partition-by-dependency-chain-depth [dependency-graph vars]
  (->> (group-by (partial dependency-chain-depth dependency-graph)
                 vars)
       (sort-by first)
       (map second)))

(deftest test-partition-by-dependency-chain-depth
  (is (= '([["ns" "b"] ["ns" "d"]]
           [["ns" "c"]]
           [["ns" "a"]])
         (partition-by-dependency-chain-depth [{:dependent ["ns" "a"]
                                                :dependency ["ns" "b"]}
                                               {:dependent ["ns" "a"]
                                                :dependency ["ns" "c"]}
                                               {:dependent ["ns" "c"]
                                                :dependency ["ns" "d"]}]
                                              [["ns" "a"]
                                               ["ns" "b"]
                                               ["ns" "c"]
                                               ["ns" "d"]]))))
