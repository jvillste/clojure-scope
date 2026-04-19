(ns clojure-scope.core
  (:require [clj-kondo.core :as kondo]
            [clojure.pprint :as pprint]))

(defn analyze-folder [folder]
  (:analysis (kondo/run! {:lint [folder]
                          :skip-lint true
                          :config {:analysis true}})))

(defn var-id [{:keys [ns name]}]
  [ns name])

(defn node
  [{:keys [ns name filename row col end-row end-col defined-by] :as definition}]
  {;; :id [ns name]
   :namespace (str ns)
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
                   (sort-by (juxt :filename :row :col))
                   vec)
        edges (->> var-usages
                   (keep (fn [usage]
                           (when-let [source-var (:from-var usage)]
                             (let [source-id [(:from usage) source-var]
                                   target-id [(:to usage) (:name usage)]]
                               (when (and (internal-var? source-id)
                                          (internal-var? target-id))
                                 (edge usage))))))
                   (sort-by (juxt :from :to :filename :row :col))
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

(comment
  (var-dependency-graph "src")
  (var-dependency-graph "/Users/jukka/google-drive/src/mappa/src")

  )
