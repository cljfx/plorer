(ns cljfx.plorer-test
  (:require [cljfx.plorer :as plorer]
            [clojure.test :as t :refer [deftest is testing]])
  (:import [javafx.application Platform]
           [javafx.scene Group Scene SubScene]
           [javafx.scene.layout VBox]
           [javafx.scene.text Text]
           [javafx.stage Stage Window]))

(defn- start-fx-runtime! []
  (let [started? (promise)]
    (try
      (Platform/startup #(deliver started? true))
      (catch IllegalStateException _
        (deliver started? true)))
    @started?))

(defn- fx-sync [f]
  (if (Platform/isFxApplicationThread)
    (f)
    (let [result (promise)]
      (Platform/runLater
        (bound-fn []
          (deliver result
                   (try
                     [::ok (f)]
                     (catch Throwable t
                       [::err t])))))
      (let [[status value] @result]
        (case status
          ::ok value
          ::err (throw value))))))

(defn- group [& children]
  (let [group (Group.)]
    (doseq [child children]
      (.add (.getChildren group) child))
    group))

(deftest props-reads-supported-properties
  (let [el (doto (Text. "Hello")
             (.setId "greeting"))]
    (testing "enumeration exposes supported keys and their current values"
      (let [props (plorer/props el)]
        (is (= "Hello" (:text props)))
        (is (= "greeting" (:id props)))
        (is (= true (:visible props)))
        (is (contains? props :text))
        (is (contains? props :id))
        (is (contains? props :visible))))
    (testing "unsupported keys are absent"
      (is (not (contains? (plorer/props el) :foo))))))

(deftest props-preserves-nil-values
  (let [el (Text. "Hello")
        props (plorer/props el)]
    (is (contains? props :id))
    (is (nil? (:id props)))))

(deftest props-only-limits-reads
  (let [el (Text. "Hello")
        props (plorer/props el :only [:text :id :foo])]
    (is (= {:text "Hello"
            :id nil}
           props))
    (is (not (contains? props :foo)))))

(deftest props-includes-list-backed-properties
  (let [el (Text. "Hello")
        props (plorer/props el :only [:style-class])]
    (is (contains? props :style-class))
    (is (vector? (vec (:style-class props))))))

(deftest props-work-off-ui-thread
  (let [el (doto (Text. "Hello")
             (.setId "greeting"))
        result @(future (plorer/props el :only [:text :id]))]
    (is (= {:text "Hello"
            :id "greeting"}
           result))))

(deftest props-rethrows-ui-thread-errors
  (is (thrown? NullPointerException
               (plorer/props nil))))

(deftest tree-on-leaf-node
  (let [el (Text. "Hello")]
    (is (= {:el el
            :children []}
           (plorer/tree el)))
    (is (not (contains? (plorer/tree el) :props)))
    (is (= {:el el}
           (plorer/tree el :depth 0)))))

(deftest tree-on-parent-preserves-order
  (let [child-1 (Text. "One")
        child-2 (Text. "Two")
        root (group child-1 child-2)]
    (is (= {:el root
            :children [{:el child-1 :children []}
                       {:el child-2 :children []}]}
           (plorer/tree root)))))

(deftest tree-honors-depth
  (let [grandchild (Text. "Grandchild")
        child (group grandchild)
        root (group child)]
    (is (= {:el root}
           (plorer/tree root :depth 0)))
    (is (= {:el root
            :children [{:el child}]}
           (plorer/tree root :depth 1)))))

(deftest tree-includes-props-when-requested
  (let [child (doto (Text. "Child")
                (.setId "child"))
        root (doto (group child)
               (.setId "root"))]
    (is (= {:el root
            :props {:id "root"}
            :children [{:el child
                        :props {:id "child"}
                        :children []}]}
           (plorer/tree root :props [:id])))))

(deftest tree-traverses-subscene-root
  (let [sub-root (Text. "Inside")
        sub-scene (SubScene. (group sub-root) 100 100)]
    (is (= {:el sub-scene
            :children [{:el (.getRoot sub-scene)
                        :children [{:el sub-root
                                    :children []}]}]}
           (plorer/tree sub-scene)))))

(deftest tree-works-off-ui-thread
  (let [child (Text. "Child")
        root (group child)
        result @(future (plorer/tree root :props [:visible]))]
    (is (= {:el root
            :props {:visible true}
            :children [{:el child
                        :props {:visible true}
                        :children []}]}
           result))))

(deftest root-children-include-open-windows
  (let [stage (fx-sync
                #(doto (Stage.)
                   (.setScene (Scene. (group)))
                   (.show)))]
    (try
      (is (some #{stage} (#'plorer/-children @#'plorer/ROOT)))
      (finally
        (fx-sync #(.close stage))))))

(deftest query-all-supports-recursive-and-direct-traversal
  (let [stage (fx-sync
                #(let [nested-text (doto (Text. "Nested")
                                     (.setId "nested"))
                       direct-text (doto (Text. "Direct")
                                     (.setId "direct"))
                       name-text (doto (Text. "Name")
                                   (.setId "name"))
                       nested-box (doto (VBox. 0.0)
                                    (.setId "nested-box"))
                       root (doto (VBox. 0.0)
                              (.setId "assets"))]
                   (.add (.getChildren nested-box) nested-text)
                   (.add (.getStyleClass root) "container")
                   (.addAll (.getChildren root) [direct-text nested-box name-text])
                   (.add (.getStyleClass direct-text) "primary")
                   (doto (Stage.)
                     (.setScene (Scene. root))
                     (.show))))]
    (try
      (let [root (.getRoot (.getScene stage))
            direct-text (first (.getChildren ^VBox root))
            nested-box (second (.getChildren ^VBox root))
            nested-text (first (.getChildren ^VBox nested-box))
            name-text (.get (.getChildren ^VBox root) 2)]
        (is (= [stage] (plorer/all > Stage)))
        (is (= [stage] (plorer/all Window)))
        (is (= [root] (plorer/all "#assets")))
        (is (= [root] (plorer/all ".container")))
        (is (= [root] (plorer/all "#assets.container")))
        (is (= [direct-text nested-text name-text] (plorer/all Text)))
        (is (= [direct-text] (plorer/all VBox > {:fx.plorer/class Text :text "Direct"})))
        (is (= [nested-text] (plorer/all "#nested")))
        (is (= [name-text] (plorer/all {:fx.plorer/class Text :id "name"})))
        (is (= [direct-text] (plorer/all {:fx.plorer/style-classes #{"primary"}})))
        (is (= [root] (plorer/all * > {:id "assets"})))
        (is (= [direct-text] (plorer/all {:fx.plorer/pred #(instance? Text %)
                                          :text "Direct"}))))
      (finally
        (fx-sync #(.close stage))))))

(deftest query-chaining-does-not-match-roots-or-duplicate-descendants
  (let [stage (fx-sync
                #(let [inner-box (doto (VBox. 0.0)
                                   (.setId "inner"))
                       middle-box (doto (VBox. 0.0)
                                    (.setId "middle"))
                       outer-box (doto (VBox. 0.0)
                                   (.setId "outer"))]
                   (.add (.getChildren middle-box) inner-box)
                   (.add (.getChildren outer-box) middle-box)
                   (doto (Stage.)
                     (.setScene (Scene. outer-box))
                     (.show))))]
    (try
      (let [root (.getRoot (.getScene stage))
            middle-box (first (.getChildren ^VBox root))
            inner-box (first (.getChildren ^VBox middle-box))]
        (is (= [root middle-box inner-box] (plorer/all VBox)))
        (is (= [middle-box inner-box] (plorer/all VBox VBox)))
        (is (= [middle-box inner-box] (plorer/all VBox > VBox)))
        (is (= [inner-box] (plorer/all VBox VBox VBox))))
      (finally
        (fx-sync #(.close stage))))))

(deftest query-one-returns-single-result-and-throws-on-cardinality-mismatch
  (let [stage (fx-sync
                #(let [text-1 (Text. "One")
                       text-2 (Text. "Two")
                       root (VBox. 0.0)]
                   (.addAll (.getChildren root) [text-1 text-2])
                   (doto (Stage.)
                     (.setScene (Scene. root))
                     (.show))))]
    (try
      (is (= stage (plorer/one > Stage)))
      (is (thrown-with-msg? IllegalStateException #"Expected exactly one match"
                            (plorer/one Text)))
      (is (thrown-with-msg? IllegalStateException #"Expected exactly one match"
                            (plorer/one "#missing")))
      (finally
        (fx-sync #(.close stage))))))

(deftest query-works-off-ui-thread
  (let [stage (fx-sync
                #(let [text (doto (Text. "Async")
                              (.setId "async"))
                       root (group text)]
                   (doto (Stage.)
                     (.setScene (Scene. root))
                     (.show))))]
    (try
      (is (= [(first (.getChildren ^Group (.getRoot (.getScene stage))))]
             @(future (plorer/all "#async"))))
      (finally
        (fx-sync #(.close stage))))))

(deftest query-map-selector-treats-literal-ifns-as-values
  (let [el (Object.)
        matcher-for (fn [selector prop-values]
                      (with-redefs [plorer/props (fn [_ & {:keys [only]}]
                                                   (select-keys prop-values only))]
                        ((#'plorer/matcher selector) el)))]
    (testing "literal set values use equality instead of membership"
      (is (true? (matcher-for {:value #{1 2}} {:value #{1 2}})))
      (is (false? (matcher-for {:value #{1 2}} {:value 1}))))
    (testing "literal vector values use equality instead of indexed lookup"
      (is (true? (matcher-for {:value [:a :b]} {:value [:a :b]})))
      (is (false? (matcher-for {:value [:a :b]} {:value :a}))))
    (testing "literal map values use equality instead of key lookup"
      (is (true? (matcher-for {:value {:k :v}} {:value {:k :v}})))
      (is (false? (matcher-for {:value {:k :v}} {:value :v}))))
    (testing "functions still act as predicates"
      (is (true? (matcher-for {:value string?} {:value "x"})))
      (is (false? (matcher-for {:value string?} {:value 1}))))
    (testing "vars that resolve to functions still act as predicates"
      (is (true? (matcher-for {:value #'string?} {:value "x"})))
      (is (false? (matcher-for {:value #'string?} {:value 1}))))))

(defn test-ns-hook []
  (start-fx-runtime!)
  (try
    (t/test-vars
      (sort-by (comp :line meta)
               (filter (comp :test meta)
                       (vals (ns-interns 'cljfx.plorer-test)))))
    (finally
      (Platform/exit))))
