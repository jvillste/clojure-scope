(ns clojure-scope.core
  (:require
   [clj-kondo.core :as kondo]
   [clojure-scope.line-region :as line-region]
   [clojure-scope.string-to-forms :as string-to-forms]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.set :as set]
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

(defn var-definition
  [{:keys [ns name filename row col end-row end-col defined-by] :as definition}]
  {:namespace (str ns)
   :name (str name)
   ;; :defined-by defined-by
   ;; :filename filename
   :start-line row
   ;; :col col
   :end-row end-row
   ;; :end-col end-col
   ;;:definition definition
   })

(defn vars [file-or-folder]
  (let [{:keys [var-definitions]} (analyze-folder file-or-folder)]
    (->> var-definitions
         (map var-definition)
         (sort-by (juxt :namespace :name))
         vec)))

(defn- dependency [{:keys [from from-var to name row col]}]
  {:dependent [(str from) (str from-var)]
   :dependency [(str to) (str name)]
   :line row
   :column col})

(defn var-dependencies [file-or-folder]
  (let [{:keys [var-definitions var-usages]} (analyze-folder file-or-folder)
        definitions-by-id (into {}
                                (map (juxt var-id identity))
                                var-definitions)
        internal-var? (fn [[ns name]]
                        (contains? definitions-by-id [ns name]))]
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

(defn dependencies-by-var [dependencies]
  (reduce (fn [acc {:keys [dependent dependency]}]
            (update acc dependent (fnil conj []) dependency))
          {}
          dependencies))

(defn transitive-dependencies
  "returns all wars a given var depends on direclty and indirectly"
  [root-var edges]
  (let [dependency-map (dependencies-by-var edges)]
    (loop [pending (seq (get dependency-map root-var))
           visited #{root-var}
           result #{}]
      (if-let [a-var (first pending)]
        (if (contains? visited a-var)
          (recur (rest pending) visited result)
          (recur (into (rest pending) (get dependency-map a-var))
                 (conj visited a-var)
                 (conj result a-var)))
        result))))

(deftest test-transitive-dependencies
  (is (= #{["namespace" "c"]
           ["namespace" "b"]
           ["namespace" "d"]}
         (transitive-dependencies ["namespace" "a"]
                                  [{:dependent ["namespace" "a"],
                                    :dependency ["namespace" "b"]}
                                   {:dependent ["namespace" "b"],
                                    :dependency ["namespace" "c"]}
                                   {:dependent ["namespace" "b"],
                                    :dependency ["namespace" "d"]}
                                   {:dependent ["namespace" "e"],
                                    :dependency ["namespace" "a"]}]))))

(defn sort-by-dependencies
  "orders nodes so that the ones that come last depend on the earlier
  ones."
  [nodes dependencies]
  (let [node-set (set nodes)
        dependency-map (reduce (fn [dependency-map {:keys [dependent dependency]}]
                                 (if (and (contains? node-set dependent)
                                          (contains? node-set dependency))
                                   (update dependency-map dependent (fnil conj #{}) dependency)
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
                               [{:dependent ["namespace-1" "var-1"]
                                 :dependency ["namespace-1" "var-2"]}]))))

(defn tree-lines [dependencies namespace name]
  (let [dependencies-by-var (dependencies-by-var dependencies)
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
  (is (= ["ns/a"
          "  ns/b"]
         (tree-lines [{:dependent ["other-ns" "x"]
                       :dependency ["ns" "b"]}
                      {:dependent ["ns" "a"]
                       :dependency ["ns" "b"]}]
                     "ns"
                     "a"))))

(defn print-tree
  "prints a dependency tree starting from a given var"
  [source-folder namespace name]
  (doseq [line (tree-lines (var-dependencies source-folder)
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
  (is (= {:namespace "ns",
          :name "a",
          :children [{:namespace "ns", :name "b", :children []}]}
         (tree-data [{:dependent ["other-ns" "x"]
                      :dependency ["ns" "b"]}
                     {:dependent ["ns" "a"]
                      :dependency ["ns" "b"]}]
                    "ns"
                    "a"))))

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
  (is (string/includes? (tree-html [{:dependent ["demo.core" "a"] :dependency ["demo.core" "b"]}]
                                   "demo.core"
                                   "a")
                        "button.tree-toggle"))
  (is (string/includes? (tree-html [{:dependent ["demo.core" "a"] :dependency ["demo.core" "b"]}]
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
  (is (string/includes? (tree-html [{:dependent ["demo.core" "a"] :dependency ["demo.core" "b"]}]
                                   "demo.core"
                                   "b")
                        "<span class=\"tree-node-name\">demo.core/b</span>"))
  (is (string/includes? (tree-html [{:dependent ["demo.core" "a"] :dependency ["demo.core" "b"]}
                                    {:dependent ["demo.core" "b"] :dependency ["demo.core" "c"]}]
                                   "demo.core"
                                   "a")
                        "event.shiftKey&&treeNodeName&&toggleButton.classList.contains('tree-toggle')")))

(defn create-tree-html
  "creates a single html file that visualizes the dependency tree
  starting from a given var. Each tree branch is closed by default,
  but can be opended by clicking."
  [source-folder namespace name html-file-name]
  (spit (io/file html-file-name)
        (tree-html (var-dependencies source-folder)
                   namespace name (source-code-by-var-for-folder source-folder))))

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

(defn sorted-dependencies [var-dependencies a-var]
  (sort-by-dependencies (transitive-dependencies a-var
                                                 var-dependencies)
                        var-dependencies))


(defn line-count [path]
  (with-open [rdr (io/reader path)]
    (count (line-seq rdr))))

(defn independent-vars
  "filter out vars that are required only by vars in the given var
  sequence"
  [var-dependencies vars]
  (let [var-set (set vars)]
    (filter (fn [var]
              (->> var-dependencies
                   (filter (comp #{var} :dependency))
                   (map :dependent)
                   (remove var-set)
                   (empty?)))
            vars)))

(defn distinct-var-dependencies [var-dependencies]
  (->> var-dependencies
       (map (fn [var-dependency]
              (select-keys var-dependency [:dependency :dependent])))
       (distinct)))

(deftest test-distinct-var-dependencies
  (is (= '({:dependency ["ns" "b"], :dependent ["ns" "a"]})
         (distinct-var-dependencies [{:dependent ["ns" "a"],
                                      :dependency ["ns" "b"],
                                      :line 1273,
                                      :column 11}
                                     {:dependent ["ns" "a"],
                                      :dependency ["ns" "b"],
                                      :line 1343,
                                      :column 24}]))))

(defn sorted-independent-dependencies [var-dependencies a-var]
  (let [sorted-dependencies (sorted-dependencies var-dependencies
                                                 a-var)]
    (->> sorted-dependencies
         (filter (set (independent-vars var-dependencies sorted-dependencies))))))

(defn entangled-vars
  "filter out vars that are required also by other vars than the given vars"
  [var-dependencies vars]
  (set/difference (set vars)
                  (set (independent-vars var-dependencies vars))))

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

(defn entangled-dependencies [var-dependencies a-var]
  (let [sorted-dependencies (sorted-dependencies var-dependencies
                                                 a-var)]
    (->> sorted-dependencies
         (filter (set (entangled-vars var-dependencies sorted-dependencies))))))

(comment
  (sorted-dependencies (var-dependencies "src")
                       ["clojure-scope.core" "sorted-dependencies"])

  (print-tree "src"
              "clojure-scope.core"
              "sorted-dependencies")

  (create-tree-html "src"
                    "clojure-scope.core"
                    "sorted-dependencies"
                    "temp/test.html")

  (source-code-by-var-for-folder "/Users/jukka/google-drive/src/mappa/src")




  )
