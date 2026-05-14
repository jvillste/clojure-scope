(ns ui
  (:require
   [clojure-scope.call-tree :as call-tree]
   [clojure-scope.core :as clojure-scope]
   [clojure-scope.move :as move]
   [clojure.set :as set]
   [clojure.string :as string]
   [flow-gl.graphics.font :as font]
   [flow-gl.gui.visuals :as visuals]
   [fungl.application :as application]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.layouts :as layouts]))

(defn on-click-mouse-event-handler [on-clicked _node event]
  (when (= :mouse-clicked (:type event))
    (on-clicked))
  event)

(def font (font/create-by-name "CourierNewPSMT" 30))

(defn text [string]
  (visuals/text-area string [1.0 1.0 1.0] font))

(defn box [content & [{:keys [fill-color]}]]
  (layouts/box 10
               (visuals/rectangle-2 :fill-color (or fill-color [0.0 0.0 1.0 0.5])
                                    :corner-arc-radius 20)
               content))

(defn var-view [state-atom analysis var]
  (box {:node (text (str (second var)
                         " "
                         (count (clojure-scope/immediate-dependents (:dependency-graph analysis)
                                                                    var))
                         "/"
                         (count (clojure-scope/immediate-dependencies (:dependency-graph analysis)
                                                                      var))))
        :mouse-event-handler [on-click-mouse-event-handler (fn []
                                                             (swap! state-atom assoc
                                                                    :focused-var var
                                                                    :previous-var (:focused-var @state-atom)))]}
       {:fill-color (if (= var (:previous-var @state-atom))
                      [0.2 0.2 1.0]
                      [0.0 0.0 0.0 0.0])}))

(defn scroll-pane [_content]
  (let [state-atom (dependable-atom/atom {:x 0 :y 0})]
    (fn [content]
      (layouts/transpose (:x @state-atom)
                         (:y @state-atom)
                         {:node content
                          ;; :x (:x @state-atom)
                          ;; :y (:y @state-atom)
                          :mouse-event-handler (fn [_node event]
                                                 (when (= :mouse-wheel-rotated
                                                          (:type event))
                                                   (swap! state-atom
                                                          update
                                                          (if (:horizontal? event)
                                                            :x :y)
                                                          (fn [value]
                                                            (min 0 (- value (* 10 (:precise-wheel-rotation event)))))))
                                                 event)}))))

(defn root-view [analysis _source-by-var & [{:keys [initial-focused-var]}]]
  (let [root-vars (clojure-scope/root-vars (:dependency-graph analysis)
                                           (->> (:var-definitions analysis)
                                                (remove (fn [var-definition]
                                                          (contains? clojure-scope/test-defining-form-kinds-set
                                                                     (:defined-by var-definition))))
                                                (map clojure-scope/var-definition-to-var)))
        state-atom (dependable-atom/atom {:focused-var (or initial-focused-var (first root-vars))
                                          #_["clojure-scope.core" "inspect-var"]})]
    (fn [analysis source-by-var & [_options]]
      (let [state @state-atom]
        (layouts/with-margin 20
          (layouts/center-horizontally
           (layouts/vertically-2 {:margin 10
                                  ;; :centered? true
                                  :fill-width? true}
                                 (layouts/flow (map (partial var-view state-atom analysis)
                                                    root-vars))
                                 {:node (layouts/horizontally-2 {:margin 10}
                                                                (box (layouts/with-minimum-size 500 nil
                                                                       (layouts/vertically-2 {:margin 10}

                                                                                             (for [var (clojure-scope/immediate-dependents (:dependency-graph analysis)
                                                                                                                                           (:focused-var state))]
                                                                                               [var-view state-atom analysis var]))))
                                                                (text "->")
                                                                (box (layouts/with-minimum-size 500 nil
                                                                       [var-view state-atom analysis (:focused-var state)]))
                                                                (text "->")
                                                                (box (layouts/with-minimum-size 500 nil
                                                                       (layouts/vertically-2 {:margin 10}
                                                                                             (for [var (clojure-scope/immediate-dependencies (:dependency-graph analysis)
                                                                                                                                             (:focused-var state))]
                                                                                               [var-view state-atom analysis var]))))
                                                                (box (layouts/with-minimum-size 500 nil
                                                                       (layouts/vertically-2 {:margin 10}
                                                                                             (for [var (clojure-scope/transitive-dependencies (:dependency-graph analysis)
                                                                                                                                              (:focused-var state))]
                                                                                               [var-view state-atom analysis var])))))
                                  :z 1}

                                 [scroll-pane (layouts/vertically-2 {}
                                                                    (for [row (string/split (get source-by-var (:focused-var state)) #"\n")]
                                                                      (text row)))])))))))

(declare analysis
         source-by-var)

(comment
  (do (def clj-kondo-analysis (clojure-scope/clj-kondo-analysis "src"
                                                                {:clj-kondo-config {:lint-as {'promesa.core/let 'clojure.core/let}}}))
      (def analysis (clojure-scope/analysis clj-kondo-analysis))

      (def core-analysis (clojure-scope/filter-analysis-by-namespace "clojure-scope.core" analysis))

      (def source-by-var (call-tree/source-code-by-var (into {}
                                                             (map (juxt clojure-scope/var-id identity))
                                                             (:var-definitions clj-kondo-analysis)))))
  )

(defn analysis-view []
  [root-view analysis source-by-var])

(application/def-start analysis-view)
