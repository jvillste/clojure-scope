(ns clojure-scope.clipboard
  (:import
   (java.awt Toolkit)
   (java.awt.datatransfer DataFlavor StringSelection)))

(defn slurp-plain-text-from-clipboard []
  (.getTransferData (.getContents (.getSystemClipboard (Toolkit/getDefaultToolkit))
                                  nil)
                    (DataFlavor/stringFlavor)))

(defn spit-plain-text-to-clipboard [text]
  (.setContents (.getSystemClipboard (Toolkit/getDefaultToolkit))
                (StringSelection. text)
                nil))
