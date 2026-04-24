(ns clojure-scope.core
  (:require
   [clj-kondo.core :as kondo]
   [clojure-scope.line-region :as line-region]
   [clojure-scope.string-to-forms :as string-to-forms]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]))

(defn clj-kondo-analysis [folder & [{:keys [clj-kondo-config]}]]
  (:analysis (kondo/run! {:lint [folder]
                          :skip-lint true
                          :config (merge {:analysis true}
                                         clj-kondo-config)})))

(defn var-id [{:keys [ns name]}]
  [(str ns) (str name)])

(defn var-definition
  [{:keys [ns name filename row col end-row end-col defined-by] :as definition}]
  {:namespace (str ns)
   :name (str name)
   :defined-by (str defined-by)
   ;; :filename filename
   :start-line row
   ;; :col col
   :end-line end-row
   ;; :end-col end-col
   ;;:definition definition
   })

(defn var-definiton-to-var [var-definition]
  [(:namespace var-definition)
   (:name var-definition)])

(defn- top-level-var-form? [form]
  (and (:name form)
       (string/starts-with? (:kind form) "def")))

(defn- top-level-definition-keys-by-file [file-names]
  (into {}
        (map (fn [file-name]
               [file-name (->> (slurp file-name)
                               (string-to-forms/string-to-forms)
                               (filter top-level-var-form?)
                               (map (juxt :name :end-line))
                               set)]))
        file-names))

(defn- top-level-var-definition? [top-level-definitions-by-file var-definition]
  (contains? (get top-level-definitions-by-file
                  (:filename var-definition))
             [(str (:name var-definition))
              (:end-row var-definition)]))

(defn var-definitions [clj-kondo-analysis-result]
  (let [{:keys [var-definitions]} clj-kondo-analysis-result
        top-level-definitions-by-file (top-level-definition-keys-by-file
                                       (distinct (map :filename var-definitions)))]
    (->> var-definitions
         (filter (partial top-level-var-definition?
                          top-level-definitions-by-file))
         (map var-definition)
         (sort-by (juxt :namespace :name))
         vec)))

(defn- dependency [{:keys [from from-var to name row col filename]}]
  {:dependent [(str from) (str from-var)]
   :dependency [(str to) (str name)]
   :file-name filename
   :line row
   :column col})

(defn dependncy-graph [clj-kondo-analysis-result]
  (let [{:keys [var-usages]} clj-kondo-analysis-result
        internal-vars (->> (var-definitions clj-kondo-analysis-result)
                           (map var-definiton-to-var)
                           set)
        internal-var? (fn [[namespace name]]
                        (contains? internal-vars [namespace name]))]
    (->> var-usages
         (keep (fn [usage]
                 (when-let [source-var (:from-var usage)]
                   (let [source-id [(str (:from usage)) (str source-var)]
                         target-id [(str (:to usage)) (str (:name usage))]]
                     (when (and (internal-var? source-id)
                                (internal-var? target-id))
                       (dependency usage))))))
         (distinct)
         (sort-by (juxt :dependent :dependency))
         vec)))

(defn- group-by-pair [dependency-graph key-field value-field]
  (reduce (fn [acc entry]
            (update acc
                    (key-field entry)
                    (fnil conj [])
                    (value-field entry)))
          {}
          dependency-graph))

(defn dependencies-by-var [dependency-graph]
  (group-by-pair dependency-graph :dependent :dependency))

(defn dependents-by-var [dependency-graph]
  (group-by-pair dependency-graph :dependency :dependent))

(deftest dependents-by-var-test
  (is (= {["ns" "b"] [["ns" "a"]]
          ["ns" "a"] [["ns" "c"]]}
         (dependents-by-var
          [{:dependent ["ns" "a"], :dependency ["ns" "b"]}
           {:dependent ["ns" "c"], :dependency ["ns" "a"]}]))))

(defn immediate-dependents [dependency-graph var]
  (->> dependency-graph
       (filter (fn [dependency]
                 (= var (:dependency dependency))))
       (map :dependent)
       (distinct)))

(defn immediate-dependencies [dependency-graph var]
  (->> dependency-graph
       (filter (fn [dependency]
                 (= var (:dependent dependency))))
       (map :dependency)
       (distinct)))

(defn colocated-test-vars [var-definitions var]
  (->> var-definitions
       (filter (fn [var-definition]
                 (and (= (first var)
                         (:namespace var-definition))
                      (= (:name var-definition)
                         (str "test-" (second var)))
                      (= "clojure.test/deftest" (:defined-by var-definition)))))
       (map var-definiton-to-var)))

(defn add-colocated-test-vars [var-definitions vars]
  (concat vars
          (mapcat (partial colocated-test-vars var-definitions)
                  vars)))

(deftest test-add-colocated-test-vars
  (let [var-definitions [{:namespace "demo.core"
                          :name "my-fn"
                          :defined-by "clojure.core/defn"
                          :start-line 1
                          :end-line 1}
                         {:namespace "demo.core"
                          :name "test-my-fn"
                          :defined-by "clojure.test/deftest"
                          :start-line 2
                          :end-line 2}
                         {:namespace "demo.core"
                          :name "other-fn"
                          :defined-by "clojure.core/defn"
                          :start-line 3
                          :end-line 3}
                         {:namespace "demo.core"
                          :name "test-other-fn"
                          :defined-by "clojure.core/defn"
                          :start-line 4
                          :end-line 4}
                         {:namespace "other.ns"
                          :name "test-my-fn"
                          :defined-by "clojure.test/deftest"
                          :start-line 1
                          :end-line 1}]]

    (is (= [["demo.core" "my-fn"]
            ["demo.core" "test-my-fn"]]
           (add-colocated-test-vars var-definitions
                                    [["demo.core" "my-fn"]])))

    (is (= [["demo.core" "other-fn"]]
           (add-colocated-test-vars var-definitions
                                    [["demo.core" "other-fn"]])))
    (is (= [["other.ns" "my-fn"]
            ["other.ns" "test-my-fn"]]
           (add-colocated-test-vars var-definitions
                                    [["other.ns" "my-fn"]])))
    (is (= [["demo.core" "my-fn"]
            ["demo.core" "other-fn"]
            ["demo.core" "test-my-fn"]]
           (add-colocated-test-vars var-definitions
                                    [["demo.core" "my-fn"]
                                     ["demo.core" "other-fn"]])))
    (is (= [["demo.core" "my-fn"]
            ["demo.core" "other-fn"]]
           (add-colocated-test-vars []
                                    [["demo.core" "my-fn"]
                                     ["demo.core" "other-fn"]])))

    (is (= []
           (add-colocated-test-vars var-definitions
                                    [])))))

(defn transitive-related-vars [root-var related-vars-by-var]
  (loop [pending (seq (get related-vars-by-var root-var))
         visited #{root-var}
         result #{}]
    (if-let [current-var (first pending)]
      (if (contains? visited current-var)
        (recur (rest pending) visited result)
        (recur (into (rest pending) (get related-vars-by-var current-var))
               (conj visited current-var)
               (conj result current-var)))
      result)))

(defn transitive-dependencies
  "returns all wars a given var depends on direclty and indirectly"
  [dependency-graph root-var]
  (transitive-related-vars root-var
                           (dependencies-by-var dependency-graph)))

(deftest test-transitive-dependencies
  (is (= #{["namespace" "c"]
           ["namespace" "b"]
           ["namespace" "d"]}
         (transitive-dependencies [{:dependent ["namespace" "a"],
                                    :dependency ["namespace" "b"]}
                                   {:dependent ["namespace" "b"],
                                    :dependency ["namespace" "c"]}
                                   {:dependent ["namespace" "b"],
                                    :dependency ["namespace" "d"]}
                                   {:dependent ["namespace" "e"],
                                    :dependency ["namespace" "a"]}]
                                  ["namespace" "a"]))))

(defn transitive-dependents
  "returns all wars that depend on a given var direclty and indirectly"
  [dependency-graph root-var]
  (transitive-related-vars root-var
                           (dependents-by-var dependency-graph)))

(deftest test-transitive-dependents
  (is (= #{["namespace" "a"]
           ["namespace" "b"]
           ["namespace" "e"]}
         (transitive-dependents [{:dependent ["namespace" "a"]
                                  :dependency ["namespace" "b"]}
                                 {:dependent ["namespace" "b"]
                                  :dependency ["namespace" "c"]}
                                 {:dependent ["namespace" "b"]
                                  :dependency ["namespace" "d"]}
                                 {:dependent ["namespace" "e"]
                                  :dependency ["namespace" "a"]}]
                                ["namespace" "c"]))))

(defn root-vars [dependency-graph vars]
  (let [dependents-by-var (dependents-by-var dependency-graph)]
    (->> vars
         (remove (fn [var]
                   (contains? dependents-by-var var))))))

(defn leaf-vars [dependency-graph vars]
  (let [dependencies-by-var (dependencies-by-var dependency-graph)]
    (->> vars
         (remove (fn [var]
                   (contains? dependencies-by-var var))))))

(defn sort-by-dependencies
  "orders nodes so that the ones that come last depend on the earlier
  ones."
  [dependency-graph nodes]
  (let [node-set (set nodes)
        dependency-map (reduce (fn [dependency-map {:keys [dependent dependency]}]
                                 (if (and (contains? node-set dependent)
                                          (contains? node-set dependency))
                                   (update dependency-map dependent (fnil conj #{}) dependency)
                                   dependency-map))
                               {}
                               dependency-graph)]
    (loop [remaining nodes
           ordered []
           ordered-set #{}]
      (if (empty? remaining)
        ordered
        (if-let [next-node (some (fn [node]
                                   (when (every? ordered-set
                                                 (get dependency-map node))
                                     node))
                                 remaining)]
          (recur (remove #(= % next-node) remaining)
                 (conj ordered next-node)
                 (conj ordered-set next-node))
          (throw (ex-info "Cannot sort nodes with cyclic dependency-graph"
                          {:nodes nodes
                           :dependencies dependency-graph})))))))

(deftest test-sort-by-dependencies
  (is (= [["namespace-1" "var-2"]
          ["namespace-1" "var-1"]]
         (sort-by-dependencies [{:dependent ["namespace-1" "var-1"]
                                 :dependency ["namespace-1" "var-2"]}]
                               [["namespace-1" "var-1"]
                                ["namespace-1" "var-2"]]))))


(defn sorted-implementing-vars [var-definitions dependency-graph var]
  (->> (transitive-dependencies dependency-graph var)
       (concat [var])
       (add-colocated-test-vars var-definitions)
       (sort-by-dependencies dependency-graph)))

(defn copy-clojure-form [form-name source-file target-file target-line]
  (let [matches (->> (string-to-forms/string-to-forms (slurp source-file))
                     (filter #(= form-name (:name %)))
                     vec)]
    (if (empty? matches)
      (throw (ex-info "Source form not found"
                      {:type :not-found
                       :form-name form-name
                       :file source-file}))

      (let [{:keys [start-line end-line]} (first matches)]
        (line-region/copy-line-region source-file target-file start-line end-line target-line)))))

(defn sorted-dependencies [dependncy-graph a-var]
  (sort-by-dependencies dependncy-graph
                        (transitive-dependencies dependncy-graph
                                                 a-var)))


(defn line-count [path]
  (with-open [rdr (io/reader path)]
    (count (line-seq rdr))))

(defn independent-vars
  "filter out vars that are required only by vars in the given var
  sequence"
  [dependncy-graph vars]
  (let [var-set (set vars)]
    (filter (fn [var]
              (->> dependncy-graph
                   (filter (comp #{var} :dependency))
                   (map :dependent)
                   (remove var-set)
                   (empty?)))
            vars)))

(defn distinct-dependncy-graph [dependncy-graph]
  (->> dependncy-graph
       (map (fn [var-dependency]
              (select-keys var-dependency [:dependency :dependent])))
       (distinct)))

(deftest test-distinct-dependncy-graph
  (is (= '({:dependency ["ns" "b"], :dependent ["ns" "a"]})
         (distinct-dependncy-graph [{:dependent ["ns" "a"],
                                     :dependency ["ns" "b"],
                                     :line 1273,
                                     :column 11}
                                    {:dependent ["ns" "a"],
                                     :dependency ["ns" "b"],
                                     :line 1343,
                                     :column 24}]))))

(defn sorted-independent-dependencies [dependncy-graph a-var]
  (let [sorted-dependencies (sorted-dependencies dependncy-graph
                                                 a-var)]
    (->> sorted-dependencies
         (filter (set (independent-vars dependncy-graph sorted-dependencies))))))

(defn entangled-vars
  "filter out vars that are required also by other vars than the given vars"
  [dependncy-graph vars]
  (set/difference (set vars)
                  (set (independent-vars dependncy-graph vars))))

(deftest test-entangled-vars
  (is (= #{}
         (entangled-vars [] ["ns" "a"])))

  (is (= #{}
         (entangled-vars [{:dependent ["ns" "a"]
                           :dependency ["ns" "b"]}]
                         [])))

  (is (= #{}
         (entangled-vars [{:dependent ["ns" "a"]
                           :dependency ["ns" "b"]}]
                         [["ns" "a"]
                          ["ns" "b"]])))

  (is (= #{["ns" "b"]}
         (entangled-vars [{:dependent ["other-ns" "a"]
                           :dependency ["ns" "b"]}]
                         [["ns" "b"]])))

  (is (= #{["ns" "b"]}
         (entangled-vars [{:dependent ["other-ns" "x"]
                           :dependency ["ns" "b"]}
                          {:dependent ["ns" "a"]
                           :dependency ["ns" "b"]}]
                         [["ns" "b"]]))))

(defn entangled-dependencies [dependncy-graph a-var]
  (let [sorted-dependencies (sorted-dependencies dependncy-graph
                                                 a-var)]
    (->> sorted-dependencies
         (filter (set (entangled-vars dependncy-graph sorted-dependencies))))))

(defn analysis [clj-kondo-analysis-result]
  {:dependency-graph (dependncy-graph clj-kondo-analysis-result)
   :var-definitions (var-definitions clj-kondo-analysis-result)})

(defn inspect-vars [analysis]
  {:root-vars (root-vars (:dependency-graph analysis)
                         (->> (:var-definitions analysis)
                              (remove (comp #{"cljs.test/deftest"}
                                            :defined-by))
                              (map var-definiton-to-var)))
   :tests (->> (:var-definitions analysis)
               (filter (comp #{"cljs.test/deftest"}
                             :defined-by))
               (map var-definiton-to-var))
   :leaf-vars (leaf-vars (:dependency-graph analysis)
                         (map var-definiton-to-var (:var-definitions analysis)))})

(defn inspect-var [analysis var]
  (let [immediate-dependents (immediate-dependents (:dependency-graph analysis) var)
        transitive-dependents (->> (transitive-dependents (:dependency-graph analysis) var)
                                   (add-colocated-test-vars (:var-definitions analysis)))
        root-dependents (root-vars (:dependency-graph analysis) transitive-dependents)
        middle-dependents (->> transitive-dependents
                               (remove (set immediate-dependents))
                               (remove (set root-dependents)))

        immediate-dependencies (immediate-dependencies (:dependency-graph analysis) var)
        transitive-dependencies (transitive-dependencies (:dependency-graph analysis) var)
        leaf-dependencies (leaf-vars (:dependency-graph analysis) transitive-dependencies)

        middle-dependencies (->> transitive-dependencies
                                 (remove (set immediate-dependencies))
                                 (remove (set leaf-dependencies)))]

    {:independent-dependencies (independent-vars (:dependency-graph analysis)
                                                 transitive-dependencies)
     :entangled-dependencies (entangled-vars (:dependency-graph analysis)
                                             transitive-dependencies)

     :dependents immediate-dependents
     :root-dependents (sort root-dependents)
     :middle-dependents (sort middle-dependents)
     :immediate-dependents (sort immediate-dependents)

     :immediate-dependencies (sort immediate-dependencies)
     :middle-dependencies (sort middle-dependencies)
     :leaf-dependencies (sort leaf-dependencies)}))

(comment
  (sorted-dependencies (dependncy-graph (clj-kondo-analysis "src"))
                       ["clojure-scope.core" "sorted-dependencies"])
  )
