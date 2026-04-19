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
          "(ns demo.core)\n(defn c [] 1)\n(defn b [] (c))\n(defn a [] (b) c)\n")
    (let [{:keys [nodes edges]} (sut/var-dependency-graph (.getPath dir))]
      (is (= [[ 'demo.core 'c]
              ['demo.core 'b]
              ['demo.core 'a]]
             (mapv :id nodes)))
      (is (= [{:from ['demo.core 'a] :to ['demo.core 'b] :type :call}
              {:from ['demo.core 'a] :to ['demo.core 'c] :type :ref}
              {:from ['demo.core 'b] :to ['demo.core 'c] :type :call}]
             (mapv #(select-keys % [:from :to :type]) edges))))))
