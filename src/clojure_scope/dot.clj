(ns clojure-scope.dot
  (:require
   [clojure.string :as string]
   [clojure.java.shell :as shell]))

(defn format-var-for-dot [[namespace name]]
  (str namespace "/" name))

(defn dag-to-dot [dag]
  (str "digraph DAG {\n"
       (string/join "\n"
                    (for [node dag]
                      (str " \""
                           (format-var-for-dot (:dependent node))
                           "\" -> \""
                           (format-var-for-dot (:dependency node))
                           "\";")))
       "\n}"))

(defn create-dot-file! [dependency-graph]
  (spit "temp/dag.dot"
        (dag-to-dot (distinct dependency-graph)))
  (shell/sh "dot" "-Tsvg" "-o" "output.svg" "dag.dot"
            :dir "temp/"))
