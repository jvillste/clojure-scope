(ns clojure-scope.core
  (:require
   [clj-kondo.core :as kondo]
   [clojure.pprint :as pprint]
   [clojure.test :refer [deftest is]]))

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
   ;; :row row
   ;; :col col
   ;; :end-row end-row
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

(defn create-tree-html
  "creates a single html file that visualizes the dependency tree
  starting from a given var. Each tree branch is closed by default,
  but can be opended by clicking."
  [source-folder namespace name html-file-name])

(comment
  (var-dependency-graph "src")
  (var-dependency-graph "/Users/jukka/google-drive/src/mappa/src")
  (print-tree "/Users/jukka/google-drive/src/mappa/src"
              "mappa.core"
              "init-mappa")

  )
