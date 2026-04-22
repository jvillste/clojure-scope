(ns clojure-scope.string-to-forms
  (:require
   [clojure.test :refer [deftest is]]
   [rewrite-clj.node :as node]
   [rewrite-clj.parser :as parser]))

(def ^:private ignored-top-level-tags #{:newline :whitespace})

(defn- unwrap-form-node [node]
  (loop [node node]
    (if (= :meta (node/tag node))
      (if-let [child (last (node/children node))]
        (recur child)
        node)
      node)))

(defn form-to-summary [wrapped-node]
  (let [node (unwrap-form-node wrapped-node)
        form (try
               (node/sexpr node)
               (catch Throwable _
                 nil))
        head (when (seq? form)
               (first form))
        kind (if (symbol? head)
               (name head)
               (name (node/tag node)))
        meta-map (meta wrapped-node)]
    {:kind kind
     :name (let [name-form (second form)]
             (when (symbol? name-form)
               (str name-form)))
     :start-line (:row meta-map)
     :end-line (:end-row meta-map)}))

(defn string-to-forms [string]
  (->> (parser/parse-string-all string)
       (node/children)
       (remove #(contains? ignored-top-level-tags (node/tag %)))
       (map form-to-summary)))

(deftest test-string-to-forms
  (is (= '({:kind "ns", :name "test", :start-line 2, :end-line 2}
           {:kind "vector", :name nil, :start-line 3, :end-line 3}
           {:kind "vector", :name nil, :start-line 4, :end-line 4}
           {:kind "list", :name nil, :start-line 5, :end-line 5}
           {:kind "set", :name nil, :start-line 6, :end-line 6}
           {:kind "map", :name nil, :start-line 7, :end-line 7}
           {:kind "defn", :name "hello", :start-line 9, :end-line 12}
           {:kind "defmacro", :name "foo", :start-line 14, :end-line 14}
           {:kind "defonce", :name "foo", :start-line 15, :end-line 15}
           {:kind "my-def", :name "foo", :start-line 16, :end-line 16}
           {:kind "comment", :name nil, :start-line 18, :end-line 20}
           {:kind "defn", :name "foo", :start-line 22, :end-line 24}
           {:kind "defn-", :name "bar", :start-line 26, :end-line 27})
         (string-to-forms "
(ns test (:require [foo.core :as foo]))
[]
[symbol-in-vector]
()
#{}
{}

^{:foo 1}

^{:bar 1}
(defn hello [])

(defmacro foo [])
(defonce foo (atom nil))
(foo/my-def foo [])

(comment
  (defonce foo (atom nil))
  (foo/my-def foo []))

(defn foo [root]
  (->> (node/children root)
       (remove #(contains? ignored-top-level-tags (node/tag %)))))

(defn- bar [node]
  (last (remove #(contains? #{:whitespace :newline :comment} (node/tag %)) (node/children node))))
"))))
