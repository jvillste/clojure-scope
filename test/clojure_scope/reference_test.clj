(ns clojure-scope.reference-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [clojure-scope.reference :as reference]))

(defn normalize-newlines [text]
  (string/replace text #"\r\n" "\n"))

(defn create-temp-file! [file-name contents]
  (let [directory (.toFile (java.nio.file.Files/createTempDirectory "move-form-" (make-array java.nio.file.attribute.FileAttribute 0)))
        file (io/file directory file-name)]
    (io/make-parents file)
    (spit file contents)
    file))

(defn read-file [^java.io.File file]
  (normalize-newlines (slurp file)))

(deftest rewrite-reference-test
  (testing "rewrites an unqualified reference to a qualified one"
    (let [file (create-temp-file! "apple.clj"
                                  (str "(ns apple)\n"
                                       "\n"
                                       "(bar 1)\n"))]
      (is (= (str "(ns apple)\n"
                  "\n"
                  "(bread/bar 1)\n")
             (normalize-newlines
               (reference/update-reference file 3 2 "bread" "bar"))))
      (is (= (str "(ns apple)\n"
                  "\n"
                  "(bread/bar 1)\n")
             (read-file file)))))

  (testing "rewrites an aliased reference to another alias"
    (let [file (create-temp-file! "crumb.clj"
                                  (str "(ns crumb (:require [apple :as a]))\n"
                                       "\n"
                                       "(a/bar 1)\n"))]
      (is (= (str "(ns crumb (:require [apple :as a]))\n"
                  "\n"
                  "(b/bar 1)\n")
             (normalize-newlines
               (reference/update-reference file 3 2 "b" "bar"))))))

  (testing "rewrites a fully-qualified namespace reference"
    (let [file (create-temp-file! "crumb.clj"
                                  (str "(ns crumb)\n"
                                       "\n"
                                       "(apple/bar 1)\n"))]
      (is (= (str "(ns crumb)\n"
                  "\n"
                  "(bread/bar 1)\n")
             (normalize-newlines
               (reference/update-reference file 3 2 "bread" "bar"))))))

  (testing "can make a reference unqualified"
    (let [file (create-temp-file! "crumb.clj"
                                  (str "(ns crumb)\n"
                                       "\n"
                                       "(bread/bar 1)\n"))]
      (is (= (str "(ns crumb)\n"
                  "\n"
                  "(bar 1)\n")
             (normalize-newlines
               (reference/update-reference file 3 2 nil "bar"))))))

  (testing "fails when no symbol exists at the given position"
    (let [file (create-temp-file! "crumb.clj"
                                  (str "(ns crumb)\n"
                                       "\n"
                                       "(bread/bar 1)\n"))]
      (is (thrown-with-msg?
            clojure.lang.ExceptionInfo
            #"Could not find symbol reference"
            (reference/update-reference file 3 12 "bread" "bar"))))))
