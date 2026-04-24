(ns clojure-scope.move-test
  (:require
   [clojure-scope.move :as move]
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

    (move/move-vars source-folder [["demo.source" "moved"]] "demo.target")

    (is (= (str "(ns demo.source)\n")
           (read-file directory "src/demo/source.clj")))
    (is (= (str "(ns demo.target)\n"
                "\n"
                "(defn existing [] :ok)\n"
                "\n"
                "(defn moved [] 1)")
           (read-file directory "src/demo/target.clj")))
    (is (= (str "(ns demo.caller\n"
                "  (:require [demo.source :as source] [demo.target :as target]))\n"
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

    (move/move-vars source-folder
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

    (move/move-vars source-folder [["demo.source" "moved"]] "demo.target")

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

    (move/move-vars source-folder [["demo.source" "moved"]] "demo.target")

    (is (= (str "(ns demo.target\n"
                "  (:require [other.util :as util] [demo.util :as util1]))\n"
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

    (move/move-vars source-folder [["demo.source" "moved"]] "demo.target")

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

    (move/move-vars source-folder [["demo.source" "moved"]] "demo.target")

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

    (move/move-vars source-folder [["demo.source" "moved"]] "demo.target")

    (is (= (str "(ns demo.caller\n"
                "  (:require [demo.source :as source]\n"
                "            [demo.target :as target]))\n"
                "\n"
                "(defn use-it [] (target/moved))\n")
           (read-file directory "src/demo/caller.clj")))))

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
            (move/move-vars source-folder [["demo.source" "moved"]] "demo.target"))))))
