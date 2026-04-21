(ns clojure-scope.core
  (:require
   [clj-kondo.core :as kondo]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [hiccup.core :as hiccup]
   [medley.core :as medley]))

(defn analyze-folder [folder]
  (:analysis (kondo/run! {:lint [folder]
                          :skip-lint true
                          :config {:analysis true}})))

(defn var-id [{:keys [ns name]}]
  [(str ns) (str name)])

(defn node
  [{:keys [ns name filename row col end-row end-col defined-by] :as definition}]
  {:namespace (str ns)
   :name (str name)
   ;; :defined-by defined-by
   ;; :filename filename
   :start-row row
   ;; :col col
   :end-row end-row
   ;; :end-col end-col
   ;;:definition definition
   })

(defn edge
  [{:keys [from from-var to name arity filename row col] :as usage}]
  {:from [(str from) (str from-var)]
   :to [(str to) (str name)]
   ;; :type (if (some? arity) :call :ref)
   ;; :filename filename
   ;; :row row
   ;; :col col
   ;; :usage usage
   })

(defn var-dependency-graph [folder]
  (let [{:keys [var-definitions var-usages]} (analyze-folder folder)
        definitions-by-id (into {}
                                (map (juxt var-id identity))
                                var-definitions)
        internal-var? (fn [[ns name]]
                        (contains? definitions-by-id [ns name]))
        nodes (->> var-definitions
                   (map node)
                   (sort-by (juxt :namespace :name))
                   vec)
        edges (->> var-usages
                   (keep (fn [usage]
                           (when-let [source-var (:from-var usage)]
                             (let [source-id [(str (:from usage)) (str source-var)]
                                   target-id [(str (:to usage)) (str (:name usage))]]
                               (when (and (internal-var? source-id)
                                          (internal-var? target-id))
                                 (edge usage))))))
                   (distinct)
                   (sort-by (juxt :from :to))
                   vec)]
    {:nodes nodes
     :edges edges}))

(defn definition-source [source-folder {:keys [filename row end-row]}]
  (when (and filename row)
    (let [file-path (let [file (io/file filename)]
                      (if (.isAbsolute file)
                        file
                        (io/file source-folder filename)))
          source-lines (-> file-path slurp string/split-lines)
          start-index (dec row)
          end-index (dec (or end-row row))]
      (->> source-lines
           (drop start-index)
           (take (inc (- end-index start-index)))
           (string/join "\n")))))

(defn source-code-by-var [source-folder definitions-by-id]
  (into {}
        (keep (fn [[[namespace name] definition]]
                (when-let [source (definition-source source-folder definition)]
                  [[namespace name] source])))
        definitions-by-id))

(defn source-code-by-var-for-folder [source-folder]
  (let [{:keys [var-definitions]} (analyze-folder source-folder)
        definitions-by-id (into {}
                                (map (juxt var-id identity))
                                var-definitions)]
    (source-code-by-var source-folder definitions-by-id)))

(defn -main [& args]
  (let [[folder & _] args]
    (when-not folder
      (binding [*out* *err*]
        (println "Usage: clojure -M -m clojure-scope.core <source-folder>")
        (System/exit 1)))
    (pprint/pprint (var-dependency-graph folder))))

(defn dependencies-by-var [edges]
  (reduce (fn [acc {:keys [from to]}]
            (update acc from (fnil conj []) to))
          {}
          edges))

(defn transitive-closure
  "returns all wars a given var depends on direclty and indirectly"
  [root-var edges]
  (let [dependency-map (dependencies-by-var edges)]
    (loop [pending (seq (get dependency-map root-var))
           visited #{root-var}
           result #{}]
      (if-let [var-id (first pending)]
        (if (contains? visited var-id)
          (recur (rest pending) visited result)
          (recur (into (rest pending) (get dependency-map var-id))
                 (conj visited var-id)
                 (conj result var-id)))
        result))))

(deftest test-transitive-closure
  (is (= #{["namespace" "c"]
           ["namespace" "b"]
           ["namespace" "d"]}
         (transitive-closure ["namespace" "a"]
                             [{:from ["namespace" "a"],
                               :to ["namespace" "b"]}
                              {:from ["namespace" "b"],
                               :to ["namespace" "c"]}
                              {:from ["namespace" "b"],
                               :to ["namespace" "d"]}
                              {:from ["namespace" "e"],
                               :to ["namespace" "a"]}]))))

(defn sort-by-dependencies
  "orders nodes so that the ones that come last depend on the earlier
  ones."
  [nodes dependencies]
  (let [node-set (set nodes)
        dependency-map (reduce (fn [dependency-map {:keys [from to]}]
                                 (if (and (contains? node-set from)
                                          (contains? node-set to))
                                   (update dependency-map from (fnil conj #{}) to)
                                   dependency-map))
                               {}
                               dependencies)]
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
          (throw (ex-info "Cannot sort nodes with cyclic dependencies"
                          {:nodes nodes
                           :dependencies dependencies})))))))

(deftest test-sort-by-dependencies
  (is (= [["namespace-1" "var-2"]
          ["namespace-1" "var-1"]]
         (sort-by-dependencies [["namespace-1" "var-1"]
                                ["namespace-1" "var-2"]]
                               [{:from ["namespace-1" "var-1"]
                                 :to ["namespace-1" "var-2"]}]))))

(defn tree-lines [edges namespace name]
  (let [dependencies-by-var (reduce (fn [acc {:keys [from to]}]
                                      (update acc from (fnil conj []) to))
                                    {}
                                    edges)
        line-for (fn [var-id depth]
                   (str (apply str (repeat (* 2 depth) " "))
                        (str (first var-id) "/" (second var-id))))]
    (letfn [(walk [var-id path depth]
              (if (contains? path var-id)
                [(str (line-for var-id depth) " (cycle)")]
                (into [(line-for var-id depth)]
                      (mapcat #(walk % (conj path var-id) (inc depth))
                              (sort (get dependencies-by-var var-id))))))]
      (walk [namespace name] #{} 0))))

(deftest test-tree-lines
  (is (= ["clojure-scope.core/var-dependency-graph"
          "  clojure-scope.core/analyze-folder"
          "  clojure-scope.core/edge"
          "  clojure-scope.core/node"
          "  clojure-scope.core/var-id"]
         (tree-lines [{:from ["clojure-scope.core" "-main"],
                       :to ["clojure-scope.core" "var-dependency-graph"]}
                      {:from ["clojure-scope.core" "print-tree"],
                       :to ["clojure-scope.core" "tree-lines"]}
                      {:from ["clojure-scope.core" "print-tree"],
                       :to ["clojure-scope.core" "var-dependency-graph"]}
                      {:from ["clojure-scope.core" "test-tree-lines"],
                       :to ["clojure-scope.core" "tree-lines"]}
                      {:from ["clojure-scope.core" "var-dependency-graph"],
                       :to ["clojure-scope.core" "analyze-folder"]}
                      {:from ["clojure-scope.core" "var-dependency-graph"],
                       :to ["clojure-scope.core" "edge"]}
                      {:from ["clojure-scope.core" "var-dependency-graph"],
                       :to ["clojure-scope.core" "node"]}
                      {:from ["clojure-scope.core" "var-dependency-graph"],
                       :to ["clojure-scope.core" "var-id"]}]
                     "clojure-scope.core"
                     "var-dependency-graph"))))

(defn print-tree
  "prints a dependency tree starting from a given var"
  [source-folder namespace name]
  (doseq [line (tree-lines (:edges (var-dependency-graph source-folder))
                           namespace name)]
    (println line)))


(defn tree-data
  [edges namespace name]
  (let [dependencies-by-var (dependencies-by-var edges)]
    (letfn [(walk [var-id path]
              (if (contains? path var-id)
                {:namespace (first var-id)
                 :name (second var-id)
                 :cycle? true
                 :children []}
                {:namespace (first var-id)
                 :name (second var-id)
                 :children (->> (get dependencies-by-var var-id)
                                sort
                                (map #(walk % (conj path var-id)))
                                vec)}))]
      (walk [namespace name] #{}))))

(deftest test-tree-data
  (is (= {:namespace "clojure-scope.core"
          :name "var-dependency-graph"
          :children [{:namespace "clojure-scope.core"
                      :name "analyze-folder"
                      :children []}
                     {:namespace "clojure-scope.core"
                      :name "edge"
                      :children []}
                     {:namespace "clojure-scope.core"
                      :name "node"
                      :children []}
                     {:namespace "clojure-scope.core"
                      :name "var-id"
                      :children []}]}
         (tree-data [{:from ["clojure-scope.core" "-main"]
                      :to ["clojure-scope.core" "var-dependency-graph"]}
                     {:from ["clojure-scope.core" "print-tree"]
                      :to ["clojure-scope.core" "tree-lines"]}
                     {:from ["clojure-scope.core" "print-tree"]
                      :to ["clojure-scope.core" "var-dependency-graph"]}
                     {:from ["clojure-scope.core" "test-tree-lines"]
                      :to ["clojure-scope.core" "tree-lines"]}
                     {:from ["clojure-scope.core" "var-dependency-graph"]
                      :to ["clojure-scope.core" "analyze-folder"]}
                     {:from ["clojure-scope.core" "var-dependency-graph"]
                      :to ["clojure-scope.core" "edge"]}
                     {:from ["clojure-scope.core" "var-dependency-graph"]
                      :to ["clojure-scope.core" "node"]}
                     {:from ["clojure-scope.core" "var-dependency-graph"]
                      :to ["clojure-scope.core" "var-id"]}]
                    "clojure-scope.core"
                    "var-dependency-graph"))))

(def tree-toggle-script
  "function directChildByClass(treeNode,className){return Array.from(treeNode.children).find(function(element){return element.classList.contains(className);});}
function setExpanded(button,childList,toggleIcon,expanded){if(expanded){childList.removeAttribute('hidden');if(toggleIcon){toggleIcon.textContent='▾';}button.setAttribute('aria-expanded','true');}else{childList.setAttribute('hidden','');if(toggleIcon){toggleIcon.textContent='▸';}button.setAttribute('aria-expanded','false');}}
function setTreeNodeExpanded(treeNode,expanded){const toggleButton=treeNode.querySelector(':scope > .tree-node-label > button.tree-toggle');if(!toggleButton){return;}const childList=directChildByClass(treeNode,'tree-children');if(!childList){return;}const toggleIcon=toggleButton.querySelector('.tree-toggle-icon');setExpanded(toggleButton,childList,toggleIcon,expanded);}
document.addEventListener('click',function(event){const toggleButton=event.target.closest('button.tree-toggle,button.source-toggle');if(!toggleButton){return;}const treeNode=toggleButton.closest('li.tree-node');if(!treeNode){return;}const treeNodeName=event.target.closest('.tree-node-name');if(event.shiftKey&&treeNodeName&&toggleButton.classList.contains('tree-toggle')){const childList=directChildByClass(treeNode,'tree-children');if(!childList){return;}const shouldExpand=childList.hasAttribute('hidden');setTreeNodeExpanded(treeNode,shouldExpand);childList.querySelectorAll('li.tree-node').forEach(function(descendantTreeNode){setTreeNodeExpanded(descendantTreeNode,shouldExpand);});return;}const isSourceToggle=toggleButton.classList.contains('source-toggle');const childList=directChildByClass(treeNode,isSourceToggle?'tree-source':'tree-children');if(!childList){return;}const toggleIcon=toggleButton.querySelector(isSourceToggle?'.source-toggle-icon':'.tree-toggle-icon');const isCollapsed=childList.hasAttribute('hidden');setExpanded(toggleButton,childList,toggleIcon,isCollapsed);});")

(defn tree-node->hiccup
  ([node]
   (tree-node->hiccup node {}))
  ([{:keys [namespace name cycle? children]} source-by-var]
   (let [source-code (get source-by-var [namespace name])]
     [:li.tree-node
      [:div.tree-node-label
       (if (seq children)
         [:button.tree-toggle {:type "button"
                               :aria-expanded "false"}
          [:span.tree-toggle-icon "▸"]
          [:span.tree-node-name
           (str namespace "/" name (when cycle? " (cycle)"))]]
         [:span.tree-node-name
          (str namespace "/" name (when cycle? " (cycle)"))])
       (when source-code
         [:button.source-toggle {:type "button"
                                 :aria-expanded "false"}
          [:span.source-toggle-icon "▸"]
          [:span.source-toggle-label "source"]])]
      (when source-code
        [:pre.tree-source {:hidden true} source-code])
      (when (seq children)
        (into [:ul.tree-children {:hidden true}]
              (map #(tree-node->hiccup % source-by-var) children)))])))

(defn tree-html
  ([edges namespace name]
   (tree-html edges namespace name {}))
  ([edges namespace name source-by-var]
   (let [tree-node (tree-node->hiccup (tree-data edges namespace name) source-by-var)]
     (hiccup/html
      [:html
       [:head
        [:meta {:charset "utf-8"}]
        [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
        [:title "Clojure Scope Dependency Tree"]
        [:style "body{font-family:system-ui,sans-serif;margin:24px;line-height:1.4} .tree-root,.tree-children{list-style:none;margin:0;padding-left:1.25rem} .tree-node{margin:.25rem 0} .tree-node-label{display:flex;align-items:center;gap:.35rem;flex-wrap:wrap} .tree-toggle,.source-toggle{border:0;background:none;cursor:pointer;padding:0;font:inherit;display:inline-flex;align-items:center;gap:.35rem} .tree-node-name{font-family:ui-monospace,monospace} .tree-children[hidden],.tree-source[hidden]{display:none} .tree-source{margin:.25rem 0 .25rem 1.4rem;padding:.5rem;background:#f6f8fa;border-radius:.4rem;overflow:auto;white-space:pre-wrap;font-family:ui-monospace,monospace}"]]
       [:body
        [:main
         [:h1 "Clojure Scope Dependency Tree"]
         [:ul.tree-root tree-node]]]
       [:script tree-toggle-script]]))))

(deftest test-tree-html
  (is (string/includes? (tree-html [{:from ["demo.core" "a"] :to ["demo.core" "b"]}]
                                   "demo.core"
                                   "a")
                        "button.tree-toggle"))
  (is (string/includes? (tree-html [{:from ["demo.core" "a"] :to ["demo.core" "b"]}]
                                   "demo.core"
                                   "a")
                        "<span class=\"tree-toggle-icon\">▸</span><span class=\"tree-node-name\">demo.core/a</span>"))
  (is (string/includes? (tree-html []
                                   "demo.core"
                                   "a"
                                   {["demo.core" "a"] "(defn a [] :ok)"})
                        "button.source-toggle"))
  (is (string/includes? (tree-html []
                                   "demo.core"
                                   "a"
                                   {["demo.core" "a"] "(defn a [] :ok)"})
                        "(defn a [] :ok)"))
  (is (string/includes? (tree-html [{:from ["demo.core" "a"] :to ["demo.core" "b"]}]
                                   "demo.core"
                                   "b")
                        "<span class=\"tree-node-name\">demo.core/b</span>"))
  (is (string/includes? (tree-html [{:from ["demo.core" "a"] :to ["demo.core" "b"]}
                                    {:from ["demo.core" "b"] :to ["demo.core" "c"]}]
                                   "demo.core"
                                   "a")
                        "event.shiftKey&&treeNodeName&&toggleButton.classList.contains('tree-toggle')")))

(defn create-tree-html
  "creates a single html file that visualizes the dependency tree
  starting from a given var. Each tree branch is closed by default,
  but can be opended by clicking."
  [source-folder namespace name html-file-name]
  (let [{:keys [edges]} (var-dependency-graph source-folder)]
    (spit (io/file html-file-name)
          (tree-html edges namespace name (source-code-by-var-for-folder source-folder)))))

(comment
  (var-dependency-graph "src")
  (var-dependency-graph "/Users/jukka/google-drive/src/mappa/src")
  (print-tree "/Users/jukka/google-drive/src/mappa/src"
              "mappa.core"
              "init-mappa")

  (create-tree-html "/Users/jukka/google-drive/src/mappa/src"
                    "mappa.core"
                    "init-mappa"
                    "temp/test.html")

  (analyze-folder "/Users/jukka/google-drive/src/mappa/src")


  (source-code-by-var-for-folder "/Users/jukka/google-drive/src/mappa/src")

  (let [var-dependency-graph (var-dependency-graph "/Users/jukka/google-drive/src/mappa/src")
        edges (->> var-dependency-graph
                   :edges
                   (filter (fn [edge]
                             (and (= "mappa.core" (first (:to edge)))
                                  (= "mappa.core" (first (:from edge)))))))
        callers (->> edges
                     (group-by :to)
                     (medley/map-vals (fn [edges]
                                        (distinct (map second (map :from edges)))))
                     (medley/map-keys second))
        calls (->> edges
                   (group-by :from)
                   (medley/map-vals (fn [edges]
                                      (distinct (map second (map :to edges)))))
                   (medley/map-keys second))
        reference-counts (->> edges
                              (group-by :to)
                              (medley/map-vals count)
                              (medley/map-keys second)
                              (sort-by (juxt second first))
                              (into {}))
        tags (medley/map-vals :tags (medley/index-by :var (edn/read-string (slurp "/Users/jukka/google-drive/src/mappa/temp/core-var-tags.edn"))))
        nodes (for [node (->> (:nodes var-dependency-graph)
                              (filter (comp #{"mappa.core"} :namespace))
                              (filter (comp #{"create-and-write-vertex-buffer"} :name)))]

                {:name (:name node)
                 ;; :reference-count (get reference-counts (:name node))
                 :callers (get callers (:name node))
                 :calls (get calls (:name node))
                 :tags-set (set (get tags (:name node)))})]
    (->>  nodes
          (sort-by (comp count :callers)))
    #_(for [tag (sort (distinct (mapcat :tags-set nodes)))]
      {:tag tag
       :vars (->> nodes
                  (filter (fn [node]
                            (contains? (:tags-set node)
                                       tag))))}))


  )
