(ns clojure-scope.core
  (:require
   [clj-kondo.core :as kondo]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [hiccup.core :as hiccup]))

(defn analyze-folder [folder]
  (:analysis (kondo/run! {:lint [folder]
                          :skip-lint true
                          :config {:analysis true}})))

(defn var-id [{:keys [ns name]}]
  [ns name])

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
                             (let [source-id [(:from usage) source-var]
                                   target-id [(:to usage) (:name usage)]]
                               (when (and (internal-var? source-id)
                                          (internal-var? target-id))
                                 (edge usage))))))
                   (distinct)
                   (sort-by (juxt :from :to))
                   vec)]
    {:nodes nodes
     :edges edges}))

(defn -main [& args]
  (let [[folder & _] args]
    (when-not folder
      (binding [*out* *err*]
        (println "Usage: clojure -M -m clojure-scope.core <source-folder>")
        (System/exit 1)))
    (pprint/pprint (var-dependency-graph folder))))

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

(defn dependencies-by-var [edges]
  (reduce (fn [acc {:keys [from to]}]
            (update acc from (fnil conj []) to))
          {}
          edges))

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
  "document.addEventListener('click',function(event){const toggleButton=event.target.closest('button.tree-toggle');if(!toggleButton){return;}const treeNode=toggleButton.closest('li.tree-node');if(!treeNode){return;}const childList=Array.from(treeNode.children).find(function(element){return element.classList.contains('tree-children');});if(!childList){return;}const toggleIcon=toggleButton.querySelector('.tree-toggle-icon');const isCollapsed=childList.hasAttribute('hidden');if(isCollapsed){childList.removeAttribute('hidden');if(toggleIcon){toggleIcon.textContent='▾';}toggleButton.setAttribute('aria-expanded','true');}else{childList.setAttribute('hidden','');if(toggleIcon){toggleIcon.textContent='▸';}toggleButton.setAttribute('aria-expanded','false');}});")

(defn tree-node->hiccup [{:keys [namespace name cycle? children]}]
  [:li.tree-node
   [:div.tree-node-label
    (if (seq children)
      [:button.tree-toggle {:type "button"
                            :aria-expanded "false"}
       [:span.tree-toggle-icon "▸"]
       [:span.tree-node-name
        (str namespace "/" name (when cycle? " (cycle)"))]]
      [:span.tree-node-name
       (str namespace "/" name (when cycle? " (cycle)"))])]
   (when (seq children)
     (into [:ul.tree-children {:hidden true}]
           (map tree-node->hiccup children)))])

(defn tree-html
  [edges namespace name]
  (let [tree-node (tree-node->hiccup (tree-data edges namespace name))]
    (hiccup/html
     [:html
      [:head
       [:meta {:charset "utf-8"}]
       [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
       [:title "Clojure Scope Dependency Tree"]
       [:style "body{font-family:system-ui,sans-serif;margin:24px;line-height:1.4} .tree-root,.tree-children{list-style:none;margin:0;padding-left:1.25rem} .tree-node{margin:.25rem 0} .tree-node-label{display:flex;align-items:center;gap:.35rem} .tree-toggle{border:0;background:none;cursor:pointer;padding:0;font:inherit;display:inline-flex;align-items:center;gap:.35rem} .tree-node-name{font-family:ui-monospace,monospace} .tree-children[hidden]{display:none}"]]
      [:body
       [:main
        [:h1 "Clojure Scope Dependency Tree"]
        [:ul.tree-root tree-node]]
       [:script tree-toggle-script]]])))

(deftest test-tree-html
  (is (string/includes? (tree-html [{:from ["demo.core" "a"] :to ["demo.core" "b"]}]
                                   "demo.core"
                                   "a")
                         "button.tree-toggle"))
  (is (string/includes? (tree-html [{:from ["demo.core" "a"] :to ["demo.core" "b"]}]
                                   "demo.core"
                                   "a")
                         "<span class=\"tree-toggle-icon\">▸</span><span class=\"tree-node-name\">demo.core/a</span>"))
  (is (string/includes? (tree-html [{:from ["demo.core" "a"] :to ["demo.core" "b"]}]
                                   "demo.core"
                                   "b")
                         "<span class=\"tree-node-name\">demo.core/b</span>")))

(defn create-tree-html
  "creates a single html file that visualizes the dependency tree
  starting from a given var. Each tree branch is closed by default,
  but can be opended by clicking."
  [source-folder namespace name html-file-name]
  (spit (io/file html-file-name)
        (tree-html (:edges (var-dependency-graph source-folder)) namespace name)))

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

  )
