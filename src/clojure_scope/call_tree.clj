(ns clojure-scope.call-tree
  (:require
   [clojure-scope.core :as core]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [hiccup.core :as hiccup]))

(defn tree-data
  [edges namespace name]
  (let [dependencies-by-var (core/dependencies-by-var edges)]
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

(defn definition-source [{:keys [filename row end-row]}]
  (when (and filename row)
    (let [file-path (let [file (io/file filename)]
                      (if (.isAbsolute file)
                        file
                        (io/file filename)))
          source-lines (-> file-path slurp string/split-lines)
          start-index (dec row)
          end-index (dec (or end-row row))]
      (->> source-lines
           (drop start-index)
           (take (inc (- end-index start-index)))
           (string/join "\n")))))

(defn source-code-by-var [definitions-by-id]
  (into {}
        (keep (fn [[[namespace name] definition]]
                (when-let [source (definition-source definition)]
                  [[namespace name] source])))
        definitions-by-id))

(defn source-code-by-var-for-folder [source-folder]
  (let [{:keys [var-definitions]} (core/clj-kondo-analysis source-folder)
        definitions-by-id (into {}
                                (map (juxt core/var-id identity))
                                var-definitions)]
    (source-code-by-var definitions-by-id)))

(defn create-tree-html
  "creates a single html file that visualizes the dependency tree
  starting from a given var. Each tree branch is closed by default,
  but can be opended by clicking."
  [source-folder namespace name html-file-name]
  (spit (io/file html-file-name)
        (tree-html (core/dependency-graph (core/clj-kondo-analysis source-folder))
                   namespace
                   name
                   (source-code-by-var-for-folder source-folder))))


(defn tree-lines [dependencies namespace name]
  (let [dependencies-by-var (core/dependencies-by-var dependencies)
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
  (doseq [line (tree-lines (core/dependency-graph (core/clj-kondo-analysis source-folder))
                           namespace name)]
    (println line)))

(comment
  (create-tree-html "src"
                    "clojure-scope.call-tree"
                    "create-tree-html"
                    "temp/test.html")


  (print-tree "src"
              "clojure-scope.core"
              "sorted-transitive-dependencies")


  )
