(ns clojure-scope.core-test
  (:require [clojure-scope.core :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(defn temp-dir []
  (-> (java.nio.file.Files/createTempDirectory
       "clojure-scope-"
       (make-array java.nio.file.attribute.FileAttribute 0))
      .toFile))

(deftest var-dependency-graph-test
  (let [dir (temp-dir)
        src-dir (io/file dir "src" "demo")
        source-file (io/file src-dir "core.clj")]
    (.mkdirs src-dir)
    (spit source-file
          "(ns demo.core)
           (defn c [] 1)
           (defn b [] (c))
           (defn a [] (b) c)")
    (is (= {:nodes
            [{:namespace "demo.core", :name "a", :start-row 4, :end-row 4}
             {:namespace "demo.core", :name "b", :start-row 3, :end-row 3}
             {:namespace "demo.core", :name "c", :start-row 2, :end-row 2}],
            :edges
            [{:from ["demo.core" "a"], :to ["demo.core" "b"]}
             {:from ["demo.core" "a"], :to ["demo.core" "c"]}
             {:from ["demo.core" "b"], :to ["demo.core" "c"]}]}
           (sut/var-dependency-graph (.getPath dir))))))
