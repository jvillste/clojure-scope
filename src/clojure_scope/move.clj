(ns clojure-scope.move)

(defn move-vars
  "moves given vars to the given namespace and updates call sites and
  adds new aliases to the ns forms. vars are vectors in
  form [\"namespace\" \"name\"] The file corresponding the target
  namespace must exist. The vars are appended to the target namespace
  file in the order they are given."
  [source-folder vars target-namespace]

  )
