(ns clojure-scope.core-test
  (:require
   [clojure-scope.call-tree :as call-tree]
   [clojure-scope.core :as sut]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]))

(defn create-temp-dir []
  (-> (java.nio.file.Files/createTempDirectory
       "clojure-scope-"
       (make-array java.nio.file.attribute.FileAttribute 0))
      .toFile))

(defn write-demo-source-file [dir]
  (let [src-dir (io/file dir "src" "demo")
        source-file (io/file src-dir "core.clj")]
    (.mkdirs src-dir)
    (spit source-file
          "(ns demo.core)\n(defn c [] 1)\n(defn b [] (c))\n(defn a [] (b) c)")))

(deftest var-dependency-graph-test
  (let [dir (create-temp-dir)]
    (write-demo-source-file dir)
    (is (= [{:dependent ["demo.core" "a"], :dependency ["demo.core" "b"], :line 4, :column 12}
            {:dependent ["demo.core" "a"], :dependency ["demo.core" "c"], :line 4, :column 16}
            {:dependent ["demo.core" "b"], :dependency ["demo.core" "c"], :line 3, :column 12}]
           (map (fn [dependency]
                  (dissoc dependency :file-name))
                (sut/dependncy-graph (sut/clj-kondo-analysis (.getPath dir))))))))

(deftest var-definitions-ignore-vars-defined-inside-comment-forms
  (let [dir (create-temp-dir)
        src-dir (io/file dir "src" "demo")
        source-file (io/file src-dir "core.clj")]
    (.mkdirs src-dir)
    (spit source-file
          (string/join "\n"
                       ["(ns demo.core)"
                        "(defn helper [] 1)"
                        "(comment"
                        "  (defn hidden [] (helper)))"
                        "(defn main [] [(helper) (hidden)])"]))
    (is (= [{:namespace "demo.core", :name "helper", :defined-by "clojure.core/defn", :start-line 2, :end-line 2}
            {:namespace "demo.core", :name "main", :defined-by "clojure.core/defn", :start-line 5, :end-line 5}]
           (sut/var-definitions (sut/clj-kondo-analysis (.getPath dir)))))))

(deftest dependncy-graph-ignores-vars-defined-inside-comment-forms
  (let [dir (create-temp-dir)
        src-dir (io/file dir "src" "demo")
        source-file (io/file src-dir "core.clj")]
    (.mkdirs src-dir)
    (spit source-file
          (string/join "\n"
                       ["(ns demo.core)"
                        "(defn helper [] 1)"
                        "(comment"
                        "  (defn hidden [] (helper)))"
                        "(defn main [] [(helper) (hidden)])"]))
    (is (= [{:dependent ["demo.core" "main"], :dependency ["demo.core" "helper"], :line 5, :column 16}]
           (map (fn [dependency]
                  (dissoc dependency :file-name))
                (sut/dependncy-graph (sut/clj-kondo-analysis (.getPath dir))))))))

(deftest source-code-by-var-for-folder-test
  (let [dir (create-temp-dir)]
    (write-demo-source-file dir)
    (let [source-code-by-var (call-tree/source-code-by-var-for-folder (.getPath dir))]
      (is (= "(defn a [] (b) c)" (get source-code-by-var ["demo.core" "a"])))
      (is (= "(defn b [] (c))" (get source-code-by-var ["demo.core" "b"])))
      (is (= "(defn c [] 1)" (get source-code-by-var ["demo.core" "c"]))))))

(defn- write-lines! [path lines]
  (spit path (string/join "\n" lines)))

(defn- read-lines [path]
  (string/split-lines (slurp path)))

(deftest copy-clojure-form-copies-the-matching-top-level-form-to-the-target-line
  (let [dir (create-temp-dir)
        source-path (.getPath (io/file dir "source.clj"))
        target-path (.getPath (io/file dir "target.clj"))]
    (write-lines! source-path ["(ns example.source)"
                               "(def keep 1)"
                               "(defn copied"
                               "  []"
                               "  :ok)"
                               "(def after 2)"])
    (write-lines! target-path ["(ns example.target)"
                               "(def target 0)"])

    (is (= {:source-first-line 3
            :source-last-line 5
            :target-line 2
            :lines-copied 3}
           (sut/copy-clojure-form "copied" source-path target-path 2)))
    (is (= ["(ns example.source)"
            "(def keep 1)"
            "(defn copied"
            "  []"
            "  :ok)"
            "(def after 2)"]
           (read-lines source-path)))
    (is (= ["(ns example.target)"
            "(defn copied"
            "  []"
            "  :ok)"
            "(def target 0)"]
           (read-lines target-path)))))

(deftest copy-clojure-form-errors-when-the-form-is-missing
  (let [dir (create-temp-dir)
        source-path (.getPath (io/file dir "source.clj"))
        target-path (.getPath (io/file dir "target.clj"))]
    (write-lines! source-path ["(ns example.source)"
                               "(def present 1)"])
    (write-lines! target-path ["(ns example.target)"])

    (testing "missing form name"
      (let [ex (try
                 (sut/copy-clojure-form "missing" source-path target-path 2)
                 nil
                 (catch clojure.lang.ExceptionInfo e
                   e))]
        (is (= :not-found (:type (ex-data ex))))))))
