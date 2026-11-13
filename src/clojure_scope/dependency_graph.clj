(ns clojure-scope.dependency-graph
  (:require
   [clojure-scope.call-tree :as call-tree]
   [clojure-scope.core :as clojure-scope]
   [clojure-scope.spring-layout :as spring-layout]
   [clojure-scope.ui :as ui]
   [clojure.test :refer [deftest is testing]]
   [flow-gl.graphics.font :as font]
   [flow-gl.gui.path :as path]
   [flow-gl.gui.polygon :as polygon]
   [flow-gl.gui.scene-graph :as scene-graph]
   [flow-gl.gui.visuals :as visuals]
   [fungl.application :as application]
   [fungl.dependable-atom :as dependable-atom]
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
                             ;;:height 10
                             :x 0,
                             :y 50
                             :var ["ns" "a"]}

                            {:width 50
                             ;;:height 20
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
    #_(run! prn (map (partial scene-graph/select-node-keys [:x :y :var :width :height])  (filter :var (scene-graph/flatten (layout/layout-scene-graph (view-compiler/compile-view-calls scene-graph)
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

(def spring-graph-font (font/create-by-name "CourierNewPSMT" 40))

(defn spring-graph-text [string & [{:keys [color]}]]
  (visuals/text-area string (or color [1.0 1.0 1.0]) spring-graph-font))

(defn scale-coordinates [scale {:keys [x y]}]
  {:x (* scale x)
   :y (* scale y)})

(defn arrow [from to]
  {:children [(path/path [1.0 1.0 1.0]
                         2
                         [from
                          to])

              (polygon/polygon [1.0 1.0 1.0]
                               2
                               [1.0 1.0 1.0]
                               [;; add arrow head coordinates here
                                ])]})

(defn spring-view [dependency-graph]
  (let [layout (spring-layout/layout-dependency-graph dependency-graph
                                                      {:node-distance 50
                                                       :iterations 100})
        state-atom (dependable-atom/atom {:scale 3.0
                                          :meta-down? false})]
    (fn [dependency-graph]
      (let [state @state-atom]
        {:type :spring-view
         :children [[ui/scroll-pane {:type :spring-superimpose
                                     :node (layouts/superimpose (concat #_[(visuals/rectangle-2 :fill-color [1.0 0.1 1.0 1.0])
                                                                           #_(assoc :width 500
                                                                                    :height 500
                                                                                    )]
                                                                        (for [[var coordinates] layout]
                                                                          {:node [spring-graph-text (second var)]
                                                                           ;; (visuals/rectangle-2 :fill-color [1.0 0.1 1.0 1.0])
                                                                           ;; :width 20
                                                                           ;; ;;:height 20
                                                                           :x (* (:scale state) (:x coordinates))
                                                                           :y (* (:scale state) (:y coordinates))})
                                                                        (for [{:keys [dependent dependency]} dependency-graph]
                                                                          (path/path [1.0 1.0 1.0]
                                                                                     2
                                                                                     [(scale-coordinates (:scale state) (get layout dependent))
                                                                                      (scale-coordinates (:scale state) (get layout dependency))]))))
                                     :x 1500
                                     :y 300}]]
         :keyboard-event-handler (fn [_node event]
                                   (when (and (= (:type event) :key-pressed)
                                              (= :meta (:key event)))
                                     (swap! state-atom assoc :meta-down? true))
                                   (when (and (= (:type event) :key-released)
                                              (= :meta (:key event)))
                                     (swap! state-atom assoc :meta-down? false)))
         :can-gain-focus? true
         :mouse-event-handler (fn [_node event]
                                (if (and (= :mouse-wheel-rotated
                                            (:type event))
                                         (:meta-down? @state-atom)
                                         (or (= :going-down
                                                (:handling-phase event))
                                             (= :on-target
                                                (:handling-phase event))))
                                  (do (swap! state-atom update :scale (fn [scale]
                                                                        (+ scale (if (neg? (:wheel-rotation event))
                                                                                   -0.01
                                                                                   0.01))))
                                      :stop)
                                  event))}))))


(defn tagging-view-demo []
  [tagging-view analysis])


(application/def-start tagging-view-demo)
#_(application/def-start spring-view-demo)

(when (deref mappa/event-channel-atom)
  (clojure.core.async/>!! @mappa/event-channel-atom
                          {:type :reset}))
