(ns clojure-scope.move
  (:require
   [clojure-scope.core :as core]
   [clojure-scope.line-region :as line-region]
   [clojure-scope.reference :as reference]
   [clojure.string :as string]
   [rewrite-clj.zip :as zip]))

(defn default-alias-for-namespace [namespace-name]
  (let [segments (string/split namespace-name #"\.")
        last-segment (last segments)]
    (if (and (= last-segment "core")
             (> (count segments) 1))
      (nth segments (- (count segments) 2))
      last-segment)))

(defn unique-alias [base-alias used-namespaces-by-alias]
  (loop [index 1
         alias base-alias]
    (if (contains? used-namespaces-by-alias alias)
      (recur (inc index)
             (str base-alias index))
      alias)))

(defn namespace-file-by-name [analysis]
  (reduce (fn [files-by-namespace {:keys [name filename]}]
            (let [namespace-name (str name)
                  current-file (get files-by-namespace namespace-name)]
              (cond
                (nil? current-file)
                (assoc files-by-namespace namespace-name filename)

                (= current-file filename)
                files-by-namespace

                :else
                (throw (ex-info "Namespace is defined in multiple files."
                                {:namespace namespace-name
                                 :files [current-file filename]})))))
          {}
          (:namespace-definitions analysis)))

(defn definition-by-var-id [analysis]
  (into {}
        (map (juxt core/var-id identity))
        (:var-definitions analysis)))

(defn aliases-by-file-and-namespace [analysis]
  (reduce (fn [aliases-by-file {:keys [filename to alias]}]
            (if alias
              (assoc-in aliases-by-file [filename (str to)] (str alias))
              aliases-by-file))
          {}
          (:namespace-usages analysis)))

(defn namespaces-by-file-and-alias [analysis]
  (reduce (fn [namespaces-by-file {:keys [filename to alias]}]
            (if alias
              (assoc-in namespaces-by-file [filename (str alias)] (str to))
              namespaces-by-file))
          {}
          (:namespace-usages analysis)))

(defn validate-move-request [analysis vars target-namespace]
  (let [definitions-by-var (definition-by-var-id analysis)
        target-file-by-namespace (namespace-file-by-name analysis)]
    (when-not (= (count vars) (count (distinct vars)))
      (throw (ex-info "Requested vars must be distinct."
                      {:vars vars})))
    (doseq [var-id vars]
      (when-not (contains? definitions-by-var var-id)
        (throw (ex-info "Requested var does not exist."
                        {:var var-id}))))
    (when-not (contains? target-file-by-namespace target-namespace)
      (throw (ex-info "Target namespace file does not exist."
                      {:target-namespace target-namespace})))
    (doseq [[source-namespace _] vars]
      (when (= source-namespace target-namespace)
        (throw (ex-info "Cannot move vars into their current namespace."
                        {:target-namespace target-namespace
                         :var [source-namespace _]}))))))

(defn symbol-at-reference [file-name line column]
  (let [root-location (zip/of-string (slurp file-name))
        symbol-location (reference/find-symbol-location root-location line column)]
    (when symbol-location
      (zip/sexpr symbol-location))))

(defn reserve-alias [planning-state file-name namespace-name preferred-alias]
  (if-let [existing-alias (get-in planning-state [:aliases-by-file-and-namespace file-name namespace-name])]
    [planning-state existing-alias]
    (let [used-namespaces-by-alias (get-in planning-state [:namespaces-by-file-and-alias file-name] {})
          base-alias (or preferred-alias
                         (default-alias-for-namespace namespace-name))
          alias (if (or (nil? preferred-alias)
                        (contains? used-namespaces-by-alias preferred-alias))
                  (unique-alias base-alias used-namespaces-by-alias)
                  preferred-alias)]
      [(-> planning-state
           (assoc-in [:aliases-by-file-and-namespace file-name namespace-name] alias)
           (assoc-in [:namespaces-by-file-and-alias file-name alias] namespace-name)
           (update :alias-additions conj {:file-name file-name
                                          :namespace namespace-name
                                          :alias alias}))
       alias])))

(defn plan-external-reference-update [planning-state target-namespace moved-vars-set usage]
  (let [calling-namespace (str (:from usage))
        dependency-var [(str (:to usage)) (str (:name usage))]]
    (if (or (not (contains? moved-vars-set dependency-var))
            (contains? moved-vars-set [calling-namespace (str (:from-var usage))]))
      [planning-state nil]
      (if (= calling-namespace target-namespace)
        [planning-state {:file-name (:filename usage)
                         :line (:name-row usage)
                         :column (:name-col usage)
                         :new-namespace-alias nil
                         :new-name (str (:name usage))}]
        (let [[updated-planning-state target-alias] (reserve-alias planning-state
                                                                   (:filename usage)
                                                                   target-namespace
                                                                   nil)]
          [updated-planning-state {:file-name (:filename usage)
                                   :line (:name-row usage)
                                   :column (:name-col usage)
                                   :new-namespace-alias target-alias
                                   :new-name (str (:name usage))}])))))

(defn copied-line-for-usage [copied-var line]
  (+ (:target-start-line copied-var)
     (- line (:source-start-line copied-var))))

(defn plan-internal-reference-update [planning-state moved-vars-set copied-var usage]
  (let [dependency-namespace (str (:to usage))
        dependency-name (str (:name usage))
        dependency-var [dependency-namespace dependency-name]
        symbol (symbol-at-reference (:source-file copied-var)
                                    (:name-row usage)
                                    (:name-col usage))
        symbol-namespace (namespace symbol)
        rewritten-line (copied-line-for-usage copied-var (:name-row usage))
        rewritten-column (:name-col usage)]
    (cond
      (contains? moved-vars-set dependency-var)
      [planning-state {:file-name (:target-file copied-var)
                       :line rewritten-line
                       :column rewritten-column
                       :new-namespace-alias nil
                       :new-name dependency-name}]

      (and (nil? (:to usage))
           (= symbol-namespace "js"))
      [planning-state nil]

      (and (nil? symbol-namespace)
           (or (= dependency-namespace "clojure.core")
               (= dependency-namespace "cljs.core")))
      [planning-state nil]

      (nil? symbol-namespace)
      (let [[updated-planning-state alias] (reserve-alias planning-state
                                                          (:target-file copied-var)
                                                          dependency-namespace
                                                          nil)]
        [updated-planning-state {:file-name (:target-file copied-var)
                                 :line rewritten-line
                                 :column rewritten-column
                                 :new-namespace-alias alias
                                 :new-name dependency-name}])

      (= symbol-namespace dependency-namespace)
      [planning-state nil]

      :else
      (let [[updated-planning-state alias] (reserve-alias planning-state
                                                          (:target-file copied-var)
                                                          dependency-namespace
                                                          symbol-namespace)]
        (if (= alias symbol-namespace)
          [updated-planning-state nil]
          [updated-planning-state {:file-name (:target-file copied-var)
                                   :line rewritten-line
                                   :column rewritten-column
                                   :new-namespace-alias alias
                                   :new-name dependency-name}])))))

(defn planning-state [analysis]
  {:aliases-by-file-and-namespace (aliases-by-file-and-namespace analysis)
   :namespaces-by-file-and-alias (namespaces-by-file-and-alias analysis)
   :alias-additions []})

(defn apply-alias-additions! [alias-additions]
  (doseq [{:keys [file-name namespace alias]} alias-additions]
    (reference/add-namespace-alias file-name namespace alias)))

(defn reference-sort-key [{:keys [line column]}]
  [line column])

(defn apply-reference-updates! [reference-updates]
  (doseq [{:keys [file-name line column new-namespace-alias new-name]}
          (sort-by reference-sort-key #(compare %2 %1) reference-updates)]
    (reference/update-reference file-name
                                line
                                column
                                new-namespace-alias
                                new-name)))

(defn file-lines [path]
  (let [content (slurp path)]
    (if (empty? content)
      []
      (vec (string/split-lines content)))))

(defn copy-var! [target-file definition]
  (let [target-lines (file-lines target-file)
        copied-lines (subvec (file-lines (:filename definition))
                             (dec (:row definition))
                             (:end-row definition))
        needs-leading-blank-line (and (not (empty? target-lines))
                                      (not (string/blank? (last target-lines))))
        target-line (inc (count target-lines))
        target-start-line (+ target-line
                             (if needs-leading-blank-line 1 0))
        lines-to-insert (if needs-leading-blank-line
                          (into [""] copied-lines)
                          copied-lines)]
    (line-region/insert-lines target-file target-line lines-to-insert)
    {:source-file (:filename definition)
     :target-file target-file
     :source-start-line (:row definition)
     :source-end-line (:end-row definition)
     :target-start-line target-start-line}))

(defn copy-vars! [definitions target-file vars]
  (reduce (fn [copied-vars var-id]
            (assoc copied-vars var-id (copy-var! target-file (get definitions var-id))))
          {}
          vars))

(defn delete-vars! [definitions vars]
  (doseq [{:keys [filename row end-row]}
          (sort-by (juxt :filename :row) #(compare %2 %1) (map definitions vars))]
    (line-region/delete-line-region filename row end-row)))

(defn plan-external-reference-updates [analysis target-namespace moved-vars-set initial-planning-state]
  (reduce (fn [[planning-state reference-updates] usage]
            (let [[updated-planning-state reference-update] (plan-external-reference-update planning-state
                                                                                            target-namespace
                                                                                            moved-vars-set
                                                                                            usage)]
              [updated-planning-state
               (cond-> reference-updates
                 reference-update (conj reference-update))]))
          [initial-planning-state []]
          (:var-usages analysis)))

(defn plan-internal-reference-updates [analysis copied-vars moved-vars-set initial-planning-state]
  (reduce (fn [[planning-state reference-updates] usage]
            (let [caller-var [(str (:from usage)) (str (:from-var usage))]]
              (if-let [copied-var (get copied-vars caller-var)]
                (let [[updated-planning-state reference-update] (plan-internal-reference-update planning-state
                                                                                                moved-vars-set
                                                                                                copied-var
                                                                                                usage)]
                  [updated-planning-state
                   (cond-> reference-updates
                     reference-update (conj reference-update))])
                [planning-state reference-updates])))
          [initial-planning-state []]
          (:var-usages analysis)))

(defn move-vars
  "moves given vars to the given namespace and updates call sites and
  adds new aliases to the ns forms. vars are vectors in
  form [\"namespace\" \"name\"] The file corresponding the target
  namespace must exist. The vars are appended to the target namespace
  file in the order they are given."
  [source-folder vars target-namespace]
  (let [analysis (core/kondo-analysis source-folder)
        _ (validate-move-request analysis vars target-namespace)
        definitions (definition-by-var-id analysis)
        target-file (get (namespace-file-by-name analysis) target-namespace)
        moved-vars-set (set vars)
        initial-planning-state (planning-state analysis)
        [external-planning-state external-reference-updates] (plan-external-reference-updates analysis
                                                                                              target-namespace
                                                                                              moved-vars-set
                                                                                              initial-planning-state)]
    (apply-alias-additions! (:alias-additions external-planning-state))
    (apply-reference-updates! external-reference-updates)
    (let [copied-vars (copy-vars! definitions target-file vars)
          [internal-planning-state internal-reference-updates] (plan-internal-reference-updates analysis
                                                                                                copied-vars
                                                                                                moved-vars-set
                                                                                                external-planning-state)
          new-target-alias-additions (drop (count (:alias-additions external-planning-state))
                                           (:alias-additions internal-planning-state))]
      (apply-alias-additions! new-target-alias-additions)
      (apply-reference-updates! internal-reference-updates)
      (delete-vars! definitions vars))))
