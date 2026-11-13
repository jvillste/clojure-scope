(ns clojure-scope.dependency-graph
  (:require
   [clojure-scope.call-tree :as call-tree]
   [clojure-scope.crossing-minimization :as crossing-minimization]
   [clojure-scope.core :as clojure-scope]
   [clojure-scope.ui :as ui]
   [clojure.test :refer [deftest is testing]]
   [flow-gl.gui.path :as path]
   [flow-gl.gui.scene-graph :as scene-graph]
   [fungl.application :as application]
   [fungl.layout :as layout]
   [fungl.layouts :as layouts]
   [fungl.view-compiler :as view-compiler]
   [medley.core :as medley]))

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


(defn- line-from-node [from-node to-node]
  "returns a line from the middle of the right edge of from-node
   to the middle of the left edge of to-node"
  [{:x (+ (:x from-node)
          (:width from-node))
    :y (+ (:y from-node)
          (quot (:height from-node)
                2))}
   {:x (:x to-node)
    :y (+ (:y to-node)
          (quot (:height to-node)
                2))}])

(defn dependency-lines
  "returns lines connecting given nodes based on the dependency
  graph. Each line goes from the middle of the right edge of the
  dependent to middle the left edge of the dependency"
  [dependency-graph nodes]
  (let [node-map (medley/index-by :var nodes)]
    (->> dependency-graph
         (keep (fn [{:keys [dependent dependency]}]
                 (when-let [from-node (get node-map dependent)]
                   (when-let [to-node (get node-map dependency)]
                     (line-from-node from-node to-node)))))
         (distinct))))

(deftest test-dependency-lines
  (is (= [[{:x 50 :y 55}
           {:x 100 :y 10}]]
         (dependency-lines [{:dependent ["ns" "a"],
                             :dependency ["ns" "b"]}]

                           [{:width 50
                             :height 10
                             :x 0,
                             :y 50
                             :var ["ns" "a"]}

                            {:width 50
                             :height 20
                             :x 100,
                             :y 0
                             :var ["ns" "b"]}]))))

(defn dependency-graph-to-vars [dependency-graph]
  (->> dependency-graph
       (mapcat (fn [dependency]
                 [(:dependency dependency)
                  (:dependent dependency)]))
       (distinct)))

(comment
  (let [dependency-graph [{:dependent :a1
                           :dependency :c2}
                          {:dependent :b1
                           :dependency :a2}
                          {:dependent :c1
                           :dependency :b2}]]
    (partition-by-dependency-chain-depth dependency-graph
                                         (dependency-graph-to-vars dependency-graph)))
  )


(defn dependency-graph-view [dependency-graph vars]
  (let [scene-graph (layouts/horizontally-2 {:margin 500 :centered false}
                                            (for [vars-in-layer (->> (reverse (take 2 (partition-by-dependency-chain-depth dependency-graph vars)))
                                                                     #_(crossing-minimization/minimize-crossings dependency-graph))]
                                              (layouts/vertically-2 {:margin 5 :fill-width? true}
                                                                    (for [var vars-in-layer]
                                                                      {:node (ui/box (ui/text (second var)))
                                                                       :var var}))))]
    (run! prn (map (partial scene-graph/select-node-keys [:x :y :var :width :height])  (filter :var (scene-graph/flatten (layout/layout-scene-graph (view-compiler/compile-view-calls scene-graph)
                                                                                                                                                    Integer/MAX_VALUE
                                                                                                                                                    Integer/MAX_VALUE))))) ;; TODO: remove me

    #_scene-graph
    (layouts/superimpose scene-graph
                         (for [line (dependency-lines dependency-graph
                                                      (filter :var (scene-graph/flatten (layout/layout-scene-graph (view-compiler/compile-view-calls scene-graph)
                                                                                                                   Integer/MAX_VALUE
                                                                                                                   Integer/MAX_VALUE))))]
                           (path/path [1.0 1.0 1.0]
                                      2
                                      line)))))

(declare analysis source-by-var)

(comment
  (do (def clj-kondo-analysis (clojure-scope/clj-kondo-analysis "src"
                                                                {:clj-kondo-config {:lint-as {'promesa.core/let 'clojure.core/let}}}))
      (def analysis (clojure-scope/analysis clj-kondo-analysis))

      (def core-analysis (clojure-scope/filter-analysis-by-namespace "clojure-scope.core" analysis))

      (def source-by-var (call-tree/source-code-by-var (into {}
                                                             (map (juxt clojure-scope/var-id identity))
                                                             (:var-definitions clj-kondo-analysis)))))
  )

(defn demo-view []
  [dependency-graph-view (:dependency-graph analysis) source-by-var])

(defn tagging-view [analysis]
  [dependency-graph-view
   (:dependency-graph analysis)
   (->> (:var-definitions analysis)
        (map clojure-scope/var-definition-to-var))]
  #_(ui/text "foo"))

(comment
  (first (partition-by-dependency-chain-depth (:dependency-graph analysis)
                                              (->> (:var-definitions analysis)
                                                   (map clojure-scope/var-definition-to-var))))
  )

(defn tagging-view-demo []
  [tagging-view analysis])

(application/def-start tagging-view-demo)
