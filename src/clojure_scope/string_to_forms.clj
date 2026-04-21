(ns clojure-scope.string-to-forms
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [rewrite-clj.node :as node]
   [rewrite-clj.parser :as parser]))
(def ^:private ignored-top-level-tags #{:newline :whitespace :comment :uneval})

(def ^:const movable-kinds #{"def" "defn" "defn-" "defmacro"})

(defn- metadata-target-child [node]
  (last (remove #(contains? #{:whitespace :newline :comment} (node/tag %)) (node/children node))))

(defn- unwrap-form-node [node]
  (loop [node node]
    (if (= :meta (node/tag node))
      (if-let [child (metadata-target-child node)]
        (recur child)
        node)
      node)))

(defn- extract-name [kind form]
  (let [name-form (second form)]
    (cond
      (and (= kind "ns")
           (symbol? name-form))
      (str name-form)

      (symbol? name-form)
      (str name-form)

      :else nil)))

(defn- fallback-kind [node]
  (case (node/tag node)
    :list "list"
    :vector "vector"
    :map "map"
    :set "set"
    :fn "fn"
    :reader-macro "reader-macro"
    :token "token"
    "unknown"))

(defn form-to-summary [node]
  (let [wrapped-node node
        node (unwrap-form-node node)
        form (try
               (node/sexpr node)
               (catch Throwable _
                 nil))
        head (when (seq? form)
               (first form))
        kind (if (symbol? head)
               (name head)
               (fallback-kind node))
        meta-map (meta wrapped-node)]
    {:kind kind
     :name (when form (extract-name kind form))
     :start-line (:row meta-map)
     :end-line (:end-row meta-map)}))

(defn string-to-forms [string]
  (->> (node/children (parser/parse-string-all string))
       (remove #(contains? ignored-top-level-tags (node/tag %)))
       (map form-to-summary)))

(deftest test-string-to-forms
  (is (= '({:kind "ns", :name "test", :start-line 2, :end-line 2}
           {:kind "vector", :name nil, :start-line 3, :end-line 3}
           {:kind "list", :name nil, :start-line 4, :end-line 4}
           {:kind "set", :name nil, :start-line 5, :end-line 5}
           {:kind "map", :name nil, :start-line 6, :end-line 6}
           {:kind "defn", :name "hello", :start-line 7, :end-line 8}
           {:kind "defmacro", :name "foo", :start-line 9, :end-line 9}
           {:kind "defonce", :name "foo", :start-line 10, :end-line 10}
           {:kind "my-def", :name "foo", :start-line 11, :end-line 11}
           {:kind "defn", :name "top-level-nodes", :start-line 13, :end-line 15}
           {:kind "defn-", :name "metadata-target-child", :start-line 17, :end-line 18})
         (string-to-forms "
(ns test (:require [foo.core :as foo]))
[]
()
#{}
{}
^{:foo 1}
(defn hello [])
(defmacro foo [])
(defonce foo (atom nil))
(foo/my-def foo [])

(defn top-level-nodes [root]
  (->> (node/children root)
       (remove #(contains? ignored-top-level-tags (node/tag %)))))

(defn- metadata-target-child [node]
  (last (remove #(contains? #{:whitespace :newline :comment} (node/tag %)) (node/children node))))
"))))
