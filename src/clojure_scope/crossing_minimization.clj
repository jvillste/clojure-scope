(ns clojure-scope.crossing-minimization
  "Orders the vars inside the layers of a dependency graph so that the
  dependencies between adjacent layers cross each other as few times as
  possible. This is the crossing minimizing step of layered graph drawing:
  barycenter sweeps are alternated in both directions and followed by local
  transpositions, keeping the best ordering found. Dependencies that connect
  layers that are not adjacent and vars that are not in any layer are ignored."
  (:require
   [clojure.test :refer [deftest is testing]]))

(def ^:private default-maximum-rounds
  10)

(defn- node-layer-index
  "returns a map from a var to the index of the first layer that contains it"
  [layers]
  (reduce-kv (fn [layer-indices layer-index layer]
               (reduce (fn [layer-indices var]
                         (if (contains? layer-indices var)
                           layer-indices
                           (assoc layer-indices var layer-index)))
                       layer-indices
                       layer))
             {}
             (vec layers)))

(deftest test-node-layer-index
  (is (= {:a 0 :b 0 :c 1}
         (node-layer-index [[:a :b] [:c]])))

  (testing "keeps the first layer of a var that is in several layers"
    (is (= {:a 0}
           (node-layer-index [[:a] [:a]])))))

(defn- layer-pair-index
  "returns the index of the lower of the two adjacent layers that the
  dependency connects, or nil when the dependency does not connect two
  adjacent layers"
  [layer-indices {:keys [dependent dependency]}]
  (let [dependent-layer-index (get layer-indices dependent)
        dependency-layer-index (get layer-indices dependency)]
    (when (and dependent-layer-index
               dependency-layer-index
               (= 1 (long (Math/abs (long (- dependent-layer-index
                                             dependency-layer-index))))))
      (min dependent-layer-index dependency-layer-index))))

(deftest test-layer-pair-index
  (is (= 0
         (layer-pair-index {:a 0 :b 1}
                           {:dependent :a :dependency :b})))

  (is (= 1
         (layer-pair-index {:a 0 :b 1 :c 2}
                           {:dependent :c :dependency :b})))

  (testing "ignores dependencies between non adjacent layers"
    (is (nil? (layer-pair-index {:a 0 :b 1 :c 2}
                                 {:dependent :a :dependency :c}))))

  (testing "ignores dependencies to vars that are not in any layer"
    (is (nil? (layer-pair-index {:a 0}
                                 {:dependent :a :dependency :missing})))))

(defn- edges-by-layer-pair
  "returns a map from the index of a layer to the distinct dependencies that
  connect the layer to the layer above it"
  [dependency-graph layers]
  (let [layer-indices (node-layer-index layers)]
    (reduce (fn [edges-by-layer-index edge]
              (if-let [layer-index (layer-pair-index layer-indices edge)]
                (update edges-by-layer-index layer-index (fnil conj []) edge)
                edges-by-layer-index))
            {}
            (distinct dependency-graph))))

(deftest test-edges-by-layer-pair
  (is (= {0 [{:dependent :a1 :dependency :c2}
             {:dependent :b1 :dependency :a2}
             {:dependent :c1 :dependency :b2}]}
         (edges-by-layer-pair [{:dependent :a1 :dependency :c2}
                               {:dependent :b1 :dependency :a2}
                               {:dependent :c1 :dependency :b2}]
                              [[:a2 :b2 :c2] [:a1 :b1 :c1]])))

  (testing "ignores dependencies between non adjacent layers"
    (is (= {0 [{:dependent :b :dependency :a}]}
           (edges-by-layer-pair [{:dependent :b :dependency :a}
                                 {:dependent :c :dependency :a}]
                                [[:a] [:b] [:c]])))))

(defn- layer-positions
  "returns a map from a var in the layer to its position in the layer"
  [layer]
  (into {}
        (map-indexed (fn [position var] [var position]))
        layer))

(deftest test-layer-positions
  (is (= {:a 0 :b 1 :c 2}
         (layer-positions [:a :b :c]))))

(defn- fenwick-tree-add
  "returns the fenwick tree with the count at the one based index increased by
  one"
  [fenwick-tree index]
  (loop [fenwick-tree fenwick-tree
         index index]
    (if (< index (count fenwick-tree))
      (recur (update fenwick-tree index inc)
             (+ index (bit-and (- index) index)))
      fenwick-tree)))

(defn- fenwick-tree-prefix-count
  "returns the sum of the counts of the one based indices up to the given index"
  [fenwick-tree index]
  (loop [counted 0
         index index]
    (if (pos? index)
      (recur (+ counted (nth fenwick-tree index))
             (- index (bit-and (- index) index)))
      counted)))

(defn- count-inversions
  "returns the number of pairs of values where the earlier value is greater
  than the later value. Runs in O(n log n) by counting, for every value, the
  number of earlier values that are greater than it."
  [values]
  (let [values (vec values)
        maximum-value (reduce max 0 values)]
    (loop [remaining-values values
           seen-count 0
           fenwick-tree (vec (repeat (+ maximum-value 2) 0))
           inversion-count 0]
      (if (empty? remaining-values)
        inversion-count
        (let [one-based-position (inc (first remaining-values))]
          (recur (rest remaining-values)
                 (inc seen-count)
                 (fenwick-tree-add fenwick-tree one-based-position)
                 (+ inversion-count
                    (- seen-count
                       (fenwick-tree-prefix-count fenwick-tree
                                                  one-based-position)))))))))

(deftest test-count-inversions
  (is (= 0 (count-inversions [])))
  (is (= 0 (count-inversions [0 1 2])))
  (is (= 3 (count-inversions [2 1 0])))
  (is (= 2 (count-inversions [2 0 1])))
  (is (= 0 (count-inversions [1 1 2])))
  (is (= 8
         (count-inversions [3 1 4 1 5 9 2 6]))))

(defn- edge-position-pairs
  "returns the positions of the edges in the two layers as sorted
  [lower-position upper-position] pairs"
  [edges lower-positions upper-positions]
  (sort (map (fn [{:keys [dependent dependency]}]
               (let [[lower-var upper-var] (if (contains? lower-positions
                                                          dependent)
                                             [dependent dependency]
                                             [dependency dependent])]
                 [(get lower-positions lower-var)
                  (get upper-positions upper-var)]))
             edges)))

(deftest test-edge-position-pairs
  (is (= [[0 1] [1 0]]
         (edge-position-pairs [{:dependent :b1 :dependency :a2}
                               {:dependent :a1 :dependency :b2}]
                              {:a2 0 :b2 1}
                              {:a1 0 :b1 1}))))

(defn- crossing-count
  "returns the number of crossings between the dependencies that connect the
  layer at the given index to the layer above it"
  [edges-by-layer-index layers layer-index]
  (if-let [edges (get edges-by-layer-index layer-index)]
    (let [lower-positions (layer-positions (nth layers layer-index))
          upper-positions (layer-positions (nth layers (inc layer-index)))]
      (count-inversions (map second
                             (edge-position-pairs edges
                                                  lower-positions
                                                  upper-positions))))
    0))

(deftest test-crossing-count
  (let [edges-by-layer-index {0 [{:dependent :a1 :dependency :c2}
                                 {:dependent :b1 :dependency :a2}
                                 {:dependent :c1 :dependency :b2}]}]
    (testing "counts the crossings between two layers"
      (is (= 2
             (crossing-count edges-by-layer-index
                             [[:a2 :b2 :c2] [:a1 :b1 :c1]]
                             0))))

    (testing "counts no crossings for an ordering without crossings"
      (is (= 0
             (crossing-count edges-by-layer-index
                             [[:c2 :a2 :b2] [:a1 :b1 :c1]]
                             0))))))

(defn- total-crossing-count
  "returns the number of crossings between all adjacent layers"
  [edges-by-layer-index layers]
  (reduce + 0 (map (partial crossing-count edges-by-layer-index layers)
                   (keys edges-by-layer-index))))

(defn- neighbour-positions
  "returns a map from a var in the layer to the positions of the vars that it
  is connected to in the adjacent layer"
  [edges layer-positions other-layer-positions]
  (reduce (fn [neighbours {:keys [dependent dependency]}]
            (let [[layer-var other-var] (if (contains? layer-positions
                                                      dependent)
                                          [dependent dependency]
                                          [dependency dependent])]
              (update neighbours
                      layer-var
                      (fnil conj [])
                      (get other-layer-positions other-var))))
          {}
          edges))

(deftest test-neighbour-positions
  (is (= {:a1 [2] :b1 [0]}
         (neighbour-positions [{:dependent :a1 :dependency :c2}
                               {:dependent :b1 :dependency :a2}]
                              {:a1 0 :b1 1}
                              {:a2 0 :c2 2}))))

(defn- average
  [numbers]
  (/ (reduce + numbers) (count numbers)))

(deftest test-average
  (is (= 2 (average [1 2 3])))
  (is (= 3/2 (average [1 2]))))

(defn- order-by-neighbour-positions
  "returns the layer sorted by the average position of the neighbours of each
  var. Vars without neighbours keep their current position."
  [layer neighbours]
  (->> layer
       (map-indexed (fn [position var]
                      [var (if-let [neighbour-positions (get neighbours var)]
                             (average neighbour-positions)
                             position)]))
       (sort-by second)
       (map first)
       (vec)))

(deftest test-order-by-neighbour-positions
  (is (= [:c2 :a2 :b2]
         (order-by-neighbour-positions [:a2 :b2 :c2]
                                       {:a2 [1]
                                        :b2 [2]
                                        :c2 [0]})))

  (testing "keeps the position of vars without neighbours"
    (is (= [:a :b :c]
           (order-by-neighbour-positions [:a :b :c]
                                         {})))))

(defn- order-layer
  "returns the layers with the layer at the given index ordered by the average
  positions of its neighbours in the adjacent layer at the other index"
  [edges-by-layer-index layers layer-index other-layer-index]
  (if-let [edges (get edges-by-layer-index (min layer-index other-layer-index))]
    (let [layer (nth layers layer-index)]
      (assoc layers
             layer-index
             (order-by-neighbour-positions
              layer
              (neighbour-positions edges
                                   (layer-positions layer)
                                   (layer-positions (nth layers other-layer-index))))))
    layers))

(defn- sweep-towards-lower-layers
  "orders every layer by the positions of its neighbours in the layer above it,
  starting from the layer closest to the top"
  [edges-by-layer-index layers]
  (reduce (fn [layers layer-index]
            (order-layer edges-by-layer-index layers layer-index (inc layer-index)))
          layers
          (range (- (count layers) 2) -1 -1)))

(defn- sweep-towards-upper-layers
  "orders every layer by the positions of its neighbours in the layer below it,
  starting from the layer closest to the bottom"
  [edges-by-layer-index layers]
  (reduce (fn [layers layer-index]
            (order-layer edges-by-layer-index layers layer-index (dec layer-index)))
          layers
          (range 1 (count layers))))

(defn- swapped-layer
  "returns the layer with the vars at the two positions swapped"
  [layer first-position second-position]
  (assoc layer
         first-position (nth layer second-position)
         second-position (nth layer first-position)))

(deftest test-swapped-layer
  (is (= [:b :a :c]
         (swapped-layer [:a :b :c] 0 1))))

(defn- crossing-count-around
  "returns the number of crossings in the layer pairs that the layer at the
  given index takes part in"
  [edges-by-layer-index layers layer-index]
  (reduce + 0
          (map (partial crossing-count edges-by-layer-index layers)
               (filter (fn [pair-index]
                         (< -1 pair-index (dec (count layers))))
                       [(dec layer-index) layer-index]))))

(defn- transpose-layer
  "swaps adjacent vars in the layer whenever the swap reduces the number of
  crossings. Stops when no swap reduces them anymore."
  [edges-by-layer-index layers layer-index]
  (loop [layers layers
         position 0]
    (if (>= (inc position) (count (nth layers layer-index)))
      layers
      (let [swapped-layers (assoc layers
                                  layer-index
                                  (swapped-layer (nth layers layer-index)
                                                 position
                                                 (inc position)))]
        (if (< (crossing-count-around edges-by-layer-index swapped-layers layer-index)
               (crossing-count-around edges-by-layer-index layers layer-index))
          (recur swapped-layers position)
          (recur layers (inc position)))))))

(defn- improve-orderings
  "sweeps, transposes and keeps the best of the seen orderings until the
  maximum number of rounds is reached or a round does not reduce the number of
  crossings anymore"
  [edges-by-layer-index layers maximum-rounds]
  (loop [round 0
         current-layers layers
         best-layers layers
         best-crossing-count (total-crossing-count edges-by-layer-index layers)]
    (if (or (zero? best-crossing-count)
            (>= round maximum-rounds))
      best-layers
      (let [swept-layers (sweep-towards-upper-layers
                          edges-by-layer-index
                          (sweep-towards-lower-layers edges-by-layer-index
                                                      current-layers))
            transposed-layers (reduce (fn [layers layer-index]
                                        (transpose-layer edges-by-layer-index
                                                         layers
                                                         layer-index))
                                      swept-layers
                                      (range (count layers)))
            crossing-count (total-crossing-count edges-by-layer-index
                                                 transposed-layers)]
        (if (< crossing-count best-crossing-count)
          (recur (inc round) transposed-layers transposed-layers crossing-count)
          best-layers)))))

(defn minimize-crossings
  "returns the layers with the vars in each layer ordered so that the
  dependencies between adjacent layers cross each other as few times as
  possible"
  ([dependency-graph layers]
   (minimize-crossings dependency-graph layers default-maximum-rounds))
  ([dependency-graph layers maximum-rounds]
   (let [layers (vec (map vec layers))]
     (improve-orderings (edges-by-layer-pair dependency-graph layers)
                        layers
                        maximum-rounds))))

(deftest test-minimize-crossings
  (is (= '([:c2 :a2 :b2]
           [:a1 :b1 :c1])
         (minimize-crossings [{:dependent :a1
                              :dependency :c2}
                             {:dependent :b1
                              :dependency :a2}
                             {:dependent :c1
                              :dependency :b2}]
                            '([:a2 :b2 :c2]
                              [:a1 :b1 :c1]))))

  (testing "orders every layer pair of a graph with several layers"
    (is (= [[:e :d] [:b :c] [:a]]
           (minimize-crossings [{:dependent :b :dependency :e}
                                {:dependent :c :dependency :d}
                                {:dependent :a :dependency :c}]
                               [[:d :e]
                                [:b :c]
                                [:a]]))))

  (testing "keeps the layers as they are when there are no dependencies"
    (is (= [[:a :b] [:c]]
           (minimize-crossings []
                               [[:a :b] [:c]]))))

  (testing "ignores dependencies between non adjacent layers"
    (is (= [[:a] [:b] [:c]]
           (minimize-crossings [{:dependent :c :dependency :a}]
                               [[:a] [:b] [:c]]))))

  (testing "ignores vars that are not in any layer"
    (is (= [[:a] [:b]]
           (minimize-crossings [{:dependent :b :dependency :missing}
                                {:dependent :missing :dependency :a}]
                               [[:a] [:b]])))))
