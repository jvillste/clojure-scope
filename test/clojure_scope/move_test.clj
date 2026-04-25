(ns clojure-scope.move-test
  (:require
   [clojure-scope.move :as move]
   [clojure-scope.test-utilities :as test-utilities]
   [clojure-scope.core :as clojure-scope]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]))

(defn create-temp-dir []
  (-> (java.nio.file.Files/createTempDirectory
       "clojure-scope-move-"
       (make-array java.nio.file.attribute.FileAttribute 0))
      .toFile))

(defn write-source-file! [directory relative-path contents]
  (let [file (io/file directory relative-path)]
    (io/make-parents file)
    (spit file contents)
    file))

(defn read-file [directory relative-path]
  (-> (io/file directory relative-path)
      slurp
      (string/replace #"\r\n" "\n")))

(deftest move-one-var-updates-external-call-sites
  (let [directory (create-temp-dir)
        source-folder (.getPath (io/file directory "src"))]
    (write-source-file! directory
                        "src/demo/source.clj"
                        (str "(ns demo.source)\n"
                             "\n"
                             "(defn moved [] 1)\n"))
    (write-source-file! directory
                        "src/demo/target.clj"
                        (str "(ns demo.target)\n"
                             "\n"
                             "(defn existing [] :ok)\n"))
    (write-source-file! directory
                        "src/demo/caller.clj"
                        (str "(ns demo.caller\n"
                             "  (:require [demo.source :as source]))\n"
                             "\n"
                             "(defn use-it [] (source/moved))\n"))

    (move/move-vars (clojure-scope/clj-kondo-analysis source-folder)
                    [["demo.source" "moved"]]
                    "demo.target")

    (is (= (str "(ns demo.source)\n")
           (read-file directory "src/demo/source.clj")))
    (is (= (str "(ns demo.target)\n"
                "\n"
                "(defn existing [] :ok)\n"
                "\n"
                "(defn moved [] 1)")
           (read-file directory "src/demo/target.clj")))
    (is (= (str "(ns demo.caller\n"
                "  (:require [demo.source :as source]\n"
                "            [demo.target :as target]))\n"
                "\n"
                "(defn use-it [] (target/moved))\n")
           (read-file directory "src/demo/caller.clj")))))

(deftest move-dependent-vars-keeps-references-unqualified
  (let [directory (create-temp-dir)
        source-folder (.getPath (io/file directory "src"))]
    (write-source-file! directory
                        "src/demo/source.clj"
                        (str "(ns demo.source)\n"
                             "\n"
                             "(defn helper [] 1)\n"
                             "\n"
                             "(defn moved [] (helper))\n"))
    (write-source-file! directory
                        "src/demo/target.clj"
                        (str "(ns demo.target)\n"))

    (move/move-vars (clojure-scope/clj-kondo-analysis source-folder)
                    [["demo.source" "helper"]
                     ["demo.source" "moved"]]
                    "demo.target")

    (is (= (str "(ns demo.source)\n")
           (read-file directory "src/demo/source.clj")))
    (is (= (str "(ns demo.target)\n"
                "\n"
                "(defn helper [] 1)\n"
                "\n"
                "(defn moved [] (helper))")
           (read-file directory "src/demo/target.clj")))))

(deftest move-var-qualifies-left-behind-source-dependencies
  (let [directory (create-temp-dir)
        source-folder (.getPath (io/file directory "src"))]
    (write-source-file! directory
                        "src/demo/source.clj"
                        (str "(ns demo.source)\n"
                             "\n"
                             "(defn helper [] 1)\n"
                             "\n"
                             "(defn moved [] (helper))\n"))
    (write-source-file! directory
                        "src/demo/target.clj"
                        (str "(ns demo.target)\n"))

    (move/move-vars (clojure-scope/clj-kondo-analysis source-folder)
                    [["demo.source" "moved"]]
                    "demo.target")

    (is (= (str "(ns demo.source)\n"
                "\n"
                "(defn helper [] 1)\n")
           (read-file directory "src/demo/source.clj")))
    (is (= (str "(ns demo.target (:require [demo.source :as source]))\n"
                "\n"
                "(defn moved [] (source/helper))")
           (read-file directory "src/demo/target.clj")))))

(deftest move-var-rewrites-conflicting-aliases-in-copied-code
  (let [directory (create-temp-dir)
        source-folder (.getPath (io/file directory "src"))]
    (write-source-file! directory
                        "src/demo/util.clj"
                        (str "(ns demo.util)\n"
                             "\n"
                             "(defn answer [] 42)\n"))
    (write-source-file! directory
                        "src/other/util.clj"
                        (str "(ns other.util)\n"
                             "\n"
                             "(defn noop [] nil)\n"))
    (write-source-file! directory
                        "src/demo/source.clj"
                        (str "(ns demo.source\n"
                             "  (:require [demo.util :as util]))\n"
                             "\n"
                             "(defn moved [] (util/answer))\n"))
    (write-source-file! directory
                        "src/demo/target.clj"
                        (str "(ns demo.target\n"
                             "  (:require [other.util :as util]))\n"))

    (move/move-vars (clojure-scope/clj-kondo-analysis source-folder)
                    [["demo.source" "moved"]]
                    "demo.target")

    (is (= (str "(ns demo.target\n"
                "  (:require [other.util :as util]\n"
                "            [demo.util :as util1]))\n"
                "\n"
                "(defn moved [] (util1/answer))")
           (read-file directory "src/demo/target.clj")))))

(deftest move-var-keeps-unqualified-clojure-core-references-unqualified
  (let [directory (create-temp-dir)
        source-folder (.getPath (io/file directory "src"))]
    (write-source-file! directory
                        "src/demo/source.clj"
                        (str "(ns demo.source)\n"
                             "\n"
                             "(defn moved [] (atom 1))\n"))
    (write-source-file! directory
                        "src/demo/target.clj"
                        (str "(ns demo.target)\n"))

    (move/move-vars (clojure-scope/clj-kondo-analysis source-folder)
                    [["demo.source" "moved"]]
                    "demo.target")

    (is (= (str "(ns demo.target)\n"
                "\n"
                "(defn moved [] (atom 1))")
           (read-file directory "src/demo/target.clj")))))

(deftest move-var-keeps-javascript-references-unqualified-in-cljs-files
  (let [directory (create-temp-dir)
        source-folder (.getPath (io/file directory "src"))]
    (write-source-file! directory
                        "src/demo/source.cljs"
                        (str "(ns demo.source)\n"
                             "\n"
                             "(defn moved [] (js/Date.now))\n"))
    (write-source-file! directory
                        "src/demo/target.cljs"
                        (str "(ns demo.target)\n"))

    (move/move-vars (clojure-scope/clj-kondo-analysis source-folder)
                    [["demo.source" "moved"]]
                    "demo.target")

    (is (= (str "(ns demo.source)\n")
           (read-file directory "src/demo/source.cljs")))
    (is (= (str "(ns demo.target)\n"
                "\n"
                "(defn moved [] (js/Date.now))")
           (read-file directory "src/demo/target.cljs")))))

(deftest move-one-var-reuses-existing-target-alias-in-caller
  (let [directory (create-temp-dir)
        source-folder (.getPath (io/file directory "src"))]
    (write-source-file! directory
                        "src/demo/source.clj"
                        (str "(ns demo.source)\n"
                             "\n"
                             "(defn moved [] 1)\n"))
    (write-source-file! directory
                        "src/demo/target.clj"
                        (str "(ns demo.target)\n"))
    (write-source-file! directory
                        "src/demo/caller.clj"
                        (str "(ns demo.caller\n"
                             "  (:require [demo.source :as source]\n"
                             "            [demo.target :as target]))\n"
                             "\n"
                             "(defn use-it [] (source/moved))\n"))

    (move/move-vars (clojure-scope/clj-kondo-analysis source-folder)
                    [["demo.source" "moved"]]
                    "demo.target")

    (is (= (str "(ns demo.caller\n"
                "  (:require [demo.source :as source]\n"
                "            [demo.target :as target]))\n"
                "\n"
                "(defn use-it [] (target/moved))\n")
           (read-file directory "src/demo/caller.clj")))))

(deftest move-var-handles-operator-usages-without-name-location
  (let [directory (create-temp-dir)
        source-folder (.getPath (io/file directory "src"))]
    (write-source-file! directory
                        "src/demo/state.cljs"
                        (str "(ns demo.state)\n"
                             "\n"
                             "(def state-atom (atom {}))\n"))
    (write-source-file! directory
                        "src/demo/source.cljs"
                        (str "(ns demo.source\n"
                             "  (:require [demo.state :as state]))\n"
                             "\n"
                             "(defn left-behind [state] 1)\n"
                             "\n"
                             "(defn moved []\n"
                             "  (let [size (left-behind @state/state-atom)]\n"
                             "    size))\n"))
    (write-source-file! directory
                        "src/demo/target.cljs"
                        (str "(ns demo.target)\n"))

    (move/move-vars (clojure-scope/clj-kondo-analysis source-folder)
                    [["demo.source" "moved"]]
                    "demo.target")

    (is (= (str "(ns demo.source\n"
                "  (:require [demo.state :as state]))\n"
                "\n"
                "(defn left-behind [state] 1)\n")
           (read-file directory "src/demo/source.cljs")))
    (let [target-file (read-file directory "src/demo/target.cljs")]
      (is (string/includes? target-file "[demo.source :as source]"))
      (is (string/includes? target-file "[demo.state :as state]"))
      (is (string/includes? target-file "(defn moved []\n  (let [size (source/left-behind @state/state-atom)]\n    size))")))))

(deftest move-var-handles-core-function-usages-without-name-location
  (let [directory (create-temp-dir)
        source-folder (.getPath (io/file directory "src"))]
    (write-source-file! directory
                        "src/demo/source.cljs"
                        (str "(ns demo.source)\n"
                             "\n"
                             "(defn moved [mesh]\n"
                             "  (let [vertices (:vertices mesh)]\n"
                             "    {:min (reduce (fn [minimum vertex]\n"
                             "                    (mapv min minimum vertex))\n"
                             "                  (first vertices)\n"
                             "                  (rest vertices))}))\n"))
    (write-source-file! directory
                        "src/demo/target.cljs"
                        (str "(ns demo.target)\n"))

    (move/move-vars (clojure-scope/clj-kondo-analysis source-folder)
                    [["demo.source" "moved"]]
                    "demo.target")

    (is (= (str "(ns demo.source)\n")
           (read-file directory "src/demo/source.cljs")))
    (is (= (str "(ns demo.target)\n"
                "\n"
                "(defn moved [mesh]\n"
                "  (let [vertices (:vertices mesh)]\n"
                "    {:min (reduce (fn [minimum vertex]\n"
                "                    (mapv min minimum vertex))\n"
                "                  (first vertices)\n"
                "                  (rest vertices))}))")
           (read-file directory "src/demo/target.cljs")))))

(deftest move-vars-validates-input
  (let [directory (create-temp-dir)
        source-folder (.getPath (io/file directory "src"))]
    (write-source-file! directory
                        "src/demo/source.clj"
                        (str "(ns demo.source)\n"
                             "\n"
                             "(defn moved [] 1)\n"))

    (testing "missing target namespace file"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Target namespace file does not exist"
           (move/move-vars (clojure-scope/clj-kondo-analysis source-folder)
                           [["demo.source" "moved"]]
                           "demo.target"))))))

(deftest move-var-handles-promesa-let-correctly
  (let [directory (create-temp-dir)
        source-folder (.getPath (io/file directory "src"))]
    (write-source-file! directory
                        "src/demo/source.cljs"
                        (test-utilities/remove-indentation "(ns demo.source
                                                              (:require [demo.state :as state]
                                                                        [promesa :as promesa]))

                                                            (defn moved [mesh]
                                                              (promesa/let [state 1] state))"))
    (write-source-file! directory
                        "src/demo/target.cljs"
                        (str "(ns demo.target)\n"))

    (move/move-vars (clojure-scope/clj-kondo-analysis source-folder
                                                      {:clj-kondo-config {:lint-as {'promesa/let 'clojure.core/let}}})

                    [["demo.source" "moved"]]
                    "demo.target")

    (is (= (test-utilities/remove-indentation "(ns demo.source
                                                 (:require [demo.state :as state]
                                                           [promesa :as promesa]))
                                ")
           (read-file directory "src/demo/source.cljs")))
    (is (= (test-utilities/remove-indentation "(ns demo.target)

                                               (defn moved [mesh]
                                                 (promesa/let [state 1] state))")
           (read-file directory "src/demo/target.cljs")))))
