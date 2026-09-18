(ns cljfx.plorer-test
  (:require [cljfx.plorer :as plorer]
            [clojure.test :as t :refer [deftest is testing]])
  (:import [java.util Locale]
           [javafx.application Platform]
           [javafx.event EventHandler]
           [javafx.scene Group Scene SubScene]
           [javafx.scene.control TextField]
           [javafx.scene.input KeyCode KeyEvent MouseButton MouseEvent]
           [javafx.scene.layout VBox]
           [javafx.scene.shape Rectangle]
           [javafx.scene.text Text]
           [javafx.stage Stage Window]))

(defn- start-fx-runtime! []
  (let [started? (promise)]
    (try
      (Platform/startup (fn mark-fx-started []
                          (deliver started? true)))
      (catch IllegalStateException _
        (deliver started? true)))
    @started?))

(defn- fx-sync [f]
  (if (Platform/isFxApplicationThread)
    (f)
    (let [result (promise)]
      (Platform/runLater (bound-fn [] (deliver result (try [::ok (f)] (catch Throwable t [::err t]))) (Thread/sleep 150)))
      (let [[status value] @result]
        (case status ::ok value ::err (throw value))))))

(defn- group [& children]
  (let [group (Group.)]
    (doseq [child children]
      (.add (.getChildren group) child))
    group))

(defn- tree-els [tree]
  (map :el (tree-seq (comp seq :children) :children tree)))

(defn- platform-shortcut-key []
  (if (.startsWith (.toLowerCase (System/getProperty "os.name")) "mac")
    :meta
    :control))

(deftest mouse-input-root-requires-a-single-open-window
  (is (thrown-with-msg? IllegalStateException #"ROOT input target requires exactly one open window, got 0"
                        (plorer/mouse-press! [50 50] :primary))))

(deftest key-input-root-requires-a-single-open-window
  (is (thrown-with-msg? IllegalStateException #"ROOT input target requires exactly one open window, got 0"
                        (plorer/key-press! :enter))))

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

(deftest tree-defaults-to-root-and-accepts-keyword-first-options
  (let [stage (fx-sync
                (fn open-stage []
                  (doto (Stage.)
                    (.setScene (Scene. (group)))
                    (.show))))]
    (try
      (is (some #{stage} (tree-els (plorer/tree))))
      (is (some #{stage} (tree-els (plorer/tree :props [:id]))))
      (let [children (:children (plorer/tree :depth 1 :props [:id]))]
        (is (some #{stage} (map :el children)))
        (is (every? empty? (map :children children))))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest tree-uses-arg-count-only-for-normalization
  (is (= {:el :props
          :children []}
         (plorer/tree :props))))

(deftest root-children-include-open-windows
  (let [stage (fx-sync
                (fn open-stage []
                  (doto (Stage.)
                    (.setScene (Scene. (group)))
                    (.show))))]
    (try
      (is (some #{stage} (#'plorer/-children @#'plorer/ROOT)))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest query-all-supports-recursive-and-direct-traversal
  (let [stage (fx-sync
                (fn create-stage-with-nested-content []
                  (let [nested-text (doto (Text. "Nested")
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
                      (.show)))))]
    (try
      (let [root (.getRoot (.getScene stage))
            direct-text (first (.getChildren ^VBox root))
            nested-box (second (.getChildren ^VBox root))
            nested-text (first (.getChildren ^VBox nested-box))
            name-text (.get (.getChildren ^VBox root) 2)]
        (is (= [stage] (plorer/all > Stage)))
        (is (= [stage] (plorer/all Window)))
        (is (= [root] (plorer/all "#assets")))
        (is (= [root] (plorer/all stage "#assets")))
        (is (= [direct-text nested-text name-text] (plorer/all root Text)))
        (is (= [root] (plorer/all ".container")))
        (is (= [root] (plorer/all "#assets.container")))
        (is (= [direct-text nested-text name-text] (plorer/all Text)))
        (is (= [direct-text] (plorer/all VBox > {:fx.plorer/class Text :text "Direct"})))
        (is (= [nested-text] (plorer/all "#nested")))
        (is (= [name-text] (plorer/all {:fx.plorer/class Text :id "name"})))
        (is (= [direct-text] (plorer/all {:fx.plorer/style-classes #{"primary"}})))
        (is (= [root] (plorer/all * > {:id "assets"})))
        (is (= [direct-text] (plorer/all {:fx.plorer/pred (fn is-text? [el]
                                                            (instance? Text el))
                                          :text "Direct"}))))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest query-chaining-does-not-match-roots-or-duplicate-descendants
  (let [stage (fx-sync
                (fn create-stage-with-nested-boxes []
                  (let [inner-box (doto (VBox. 0.0)
                                    (.setId "inner"))
                        middle-box (doto (VBox. 0.0)
                                     (.setId "middle"))
                        outer-box (doto (VBox. 0.0)
                                    (.setId "outer"))]
                    (.add (.getChildren middle-box) inner-box)
                    (.add (.getChildren outer-box) middle-box)
                    (doto (Stage.)
                      (.setScene (Scene. outer-box))
                      (.show)))))]
    (try
      (let [root (.getRoot (.getScene stage))
            middle-box (first (.getChildren ^VBox root))
            inner-box (first (.getChildren ^VBox middle-box))]
        (is (= [root middle-box inner-box] (plorer/all VBox)))
        (is (= [middle-box inner-box] (plorer/all VBox VBox)))
        (is (= [middle-box inner-box] (plorer/all VBox > VBox)))
        (is (= [inner-box] (plorer/all VBox VBox VBox))))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest query-one-returns-single-result-and-throws-on-cardinality-mismatch
  (let [stage (fx-sync
                (fn create-stage-with-two-texts []
                  (let [text-1 (Text. "One")
                        text-2 (Text. "Two")
                        root (VBox. 0.0)]
                    (.addAll (.getChildren root) [text-1 text-2])
                    (doto (Stage.)
                      (.setScene (Scene. root))
                      (.show)))))]
    (try
      (is (= stage (plorer/one > Stage)))
      (is (= (first (.getChildren ^VBox (.getRoot (.getScene stage))))
             (plorer/one stage {:fx.plorer/class Text :text "One"})))
      (is (thrown-with-msg? IllegalStateException #"Expected exactly one match"
                            (plorer/one Text)))
      (is (thrown-with-msg? IllegalStateException #"Expected exactly one match"
                            (plorer/one "#missing")))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest query-works-off-ui-thread
  (let [stage (fx-sync
                (fn create-stage-with-async-text []
                  (let [text (doto (Text. "Async")
                               (.setId "async"))
                        root (group text)]
                    (doto (Stage.)
                      (.setScene (Scene. root))
                      (.show)))))]
    (try
      (is (= [(first (.getChildren ^Group (.getRoot (.getScene stage))))]
             @(future (plorer/all "#async"))))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest query-map-selector-treats-literal-ifns-as-values
  (let [el (Object.)
        matcher-for (fn matcher-for-selector [selector prop-values]
                      (with-redefs [plorer/props (fn stubbed-props [_ & {:keys [only]}]
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

(deftest query-bare-predicate-selectors-only-accept-fns-and-vars
  (is (= #:fx.plorer{:pred string?}
         (#'plorer/canonicalize-selector string?)))
  (is (= #:fx.plorer{:pred #'string?}
         (#'plorer/canonicalize-selector #'string?)))
  (is (thrown-with-msg? IllegalArgumentException #"Unsupported selector: :id"
                        (#'plorer/canonicalize-selector :id)))
  (is (thrown-with-msg? IllegalArgumentException #"Unsupported selector: #\{:a\}"
                        (#'plorer/canonicalize-selector #{:a})))
  (is (thrown-with-msg? IllegalArgumentException #"Unsupported selector: \[:a\]"
                        (#'plorer/canonicalize-selector [:a]))))

(deftest mouse-input-dispatches-to-the-picked-target
  (let [events (atom [])
        stage (fx-sync
                (fn create-stage-for-mouse-input []
                  (let [back (doto (Rectangle. 100.0 100.0)
                               (.setId "back"))
                        front (doto (Rectangle. 100.0 100.0)
                                (.setId "front"))
                        root (group back front)]
                    (.addEventHandler front MouseEvent/MOUSE_PRESSED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:pressed (.getTarget event)]))))
                    (.addEventHandler front MouseEvent/MOUSE_RELEASED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:released (.getTarget event)]))))
                    (doto (Stage.)
                      (.setScene (Scene. root 100.0 100.0))
                      (.show)))))]
    (try
      (let [front (plorer/one "#front")]
        (is (= front (plorer/mouse-press! stage [50 50] :primary)))
        (is (= front (plorer/mouse-release! stage [50 50] :primary)))
        (is (= [[:pressed front]
                [:released front]]
               @events)))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest mouse-input-release-allows-clicking-another-target
  (let [events (atom [])
        stage (fx-sync
                (fn create-stage-for-successive-clicks []
                  (let [left (doto (Rectangle. 100.0 100.0)
                               (.setId "left"))
                        right (doto (Rectangle. 100.0 100.0)
                                (.setId "right")
                                (.setTranslateX 120.0))
                        root (group left right)]
                    (.addEventHandler root MouseEvent/MOUSE_RELEASED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [(.getTarget event)
                                                              (.isPrimaryButtonDown event)
                                                              (.isMiddleButtonDown event)
                                                              (.isSecondaryButtonDown event)]))))
                    (doto (Stage.)
                      (.setScene (Scene. root 220.0 100.0))
                      (.show)))))]
    (try
      (let [left (plorer/one stage "#left")
            right (plorer/one stage "#right")]
        (doseq [button [:primary :middle :secondary]]
          (testing (name button)
            (reset! events [])
            (doseq [[target x] [[left 50] [right 170] [left 50]]]
              (is (= target (plorer/mouse-press! stage [x 50] button)))
              (is (= target (plorer/mouse-release! stage [x 50] button))))
            (is (= [[left false false false]
                    [right false false false]
                    [left false false false]]
                   @events)))))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest mouse-input-accepts-mousebutton-enum-values
  (let [events (atom [])
        stage (fx-sync
                (fn create-stage-for-mouse-button-enum-input []
                  (let [target (doto (Rectangle. 100.0 100.0)
                                 (.setId "target"))
                        root (group target)]
                    (.addEventHandler target MouseEvent/MOUSE_PRESSED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:pressed (.getButton event)]))))
                    (.addEventHandler target MouseEvent/MOUSE_RELEASED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:released (.getButton event)]))))
                    (doto (Stage.)
                      (.setScene (Scene. root 100.0 100.0))
                      (.show)))))]
    (try
      (let [target (plorer/one "#target")]
        (is (= target (plorer/mouse-press! stage [50 50] MouseButton/PRIMARY)))
        (is (= target (plorer/mouse-release! stage [50 50] MouseButton/PRIMARY)))
        (is (= [[:pressed MouseButton/PRIMARY]
                [:released MouseButton/PRIMARY]]
               @events)))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest mouse-input-accepts-scene-and-window
  (let [events (atom [])
        stage (fx-sync
                (fn create-stage-for-scene-and-window-input []
                  (let [target (doto (Rectangle. 100.0 100.0)
                                 (.setId "target"))
                        root (group target)
                        scene (Scene. root 100.0 100.0)]
                    (.addEventHandler target MouseEvent/MOUSE_PRESSED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:pressed (.getTarget event)]))))
                    (.addEventHandler target MouseEvent/MOUSE_RELEASED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:released (.getTarget event)]))))
                    (doto (Stage.)
                      (.setScene scene)
                      (.show)))))]
    (try
      (let [scene (.getScene stage)
            target (plorer/one "#target")]
        (is (= target (plorer/mouse-press! scene [50 50] :primary)))
        (is (= target (plorer/mouse-release! stage [50 50] :primary)))
        (is (= [[:pressed target]
                [:released target]]
               @events)))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest mouse-input-defaults-to-root
  (let [events (atom [])
        stage (fx-sync
                (fn create-stage-for-root-mouse-input []
                  (let [target (doto (Rectangle. 100.0 100.0)
                                 (.setId "target"))
                        root (group target)]
                    (.addEventHandler target MouseEvent/MOUSE_PRESSED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:pressed (.getTarget event)]))))
                    (.addEventHandler target MouseEvent/MOUSE_RELEASED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:released (.getTarget event)]))))
                    (doto (Stage.)
                      (.setScene (Scene. root 100.0 100.0))
                      (.show)))))]
    (try
      (let [target (plorer/one "#target")]
        (is (= target (plorer/mouse-press! [50 50] :primary)))
        (is (= target (plorer/mouse-release! [50 50] :primary)))
        (is (= [[:pressed target]
                [:released target]]
               @events)))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest mouse-input-picks-cover-without-releasing-automatically
  (let [events (atom [])
        stage (fx-sync
                (fn create-stage-for-covered-mouse-input []
                  (let [back (doto (Rectangle. 100.0 100.0)
                               (.setId "back"))
                        cover (doto (Rectangle. 100.0 100.0)
                                (.setId "cover"))
                        root (group back cover)]
                    (.addEventHandler cover MouseEvent/MOUSE_PRESSED
                                      (reify EventHandler
                                        (handle [_ _]
                                          (swap! events conj :pressed))))
                    (.addEventHandler cover MouseEvent/MOUSE_RELEASED
                                      (reify EventHandler
                                        (handle [_ _]
                                          (swap! events conj :released))))
                    (doto (Stage.)
                      (.setScene (Scene. root 100.0 100.0))
                      (.show)))))]
    (try
      (let [cover (plorer/one "#cover")]
        (is (= cover (plorer/mouse-press! stage [25 75] :primary)))
        (is (= [:pressed] @events))
        (is (= cover (plorer/mouse-release! stage [25 75] :primary)))
        (is (= [:pressed :released] @events)))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest mouse-input-press-and-release-simulate-single-click
  (let [events (atom [])
        stage (fx-sync
                (fn create-stage-for-single-click-input []
                  (let [target (doto (Rectangle. 100.0 100.0)
                                 (.setId "target"))
                        root (group target)]
                    (.addEventHandler target MouseEvent/MOUSE_CLICKED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:clicked (.getClickCount event) (.getButton event)]))))
                    (doto (Stage.)
                      (.setScene (Scene. root 100.0 100.0))
                      (.show)))))]
    (try
      (let [target (plorer/one "#target")]
        (is (= target (plorer/mouse-press! stage [50 50] :primary)))
        (is (= target (plorer/mouse-release! stage [50 50] :primary)))
        (is (= [[:clicked 1 MouseButton/PRIMARY]]
               @events)))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest mouse-input-press-and-release-simulate-double-click
  (let [events (atom [])
        stage (fx-sync
                (fn create-stage-for-double-click-input []
                  (let [target (doto (Rectangle. 100.0 100.0)
                                 (.setId "target"))
                        root (group target)]
                    (.addEventHandler target MouseEvent/MOUSE_CLICKED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:clicked (.getClickCount event) (.getButton event)]))))
                    (doto (Stage.)
                      (.setScene (Scene. root 100.0 100.0))
                      (.show)))))]
    (try
      (let [target (plorer/one "#target")]
        (is (= target (plorer/mouse-press! stage [50 50] :primary)))
        (is (= target (plorer/mouse-release! stage [50 50] :primary)))
        (is (= target (plorer/mouse-press! stage [50 50] :primary)))
        (is (= target (plorer/mouse-release! stage [50 50] :primary)))
        (is (= [[:clicked 1 MouseButton/PRIMARY]
                [:clicked 2 MouseButton/PRIMARY]]
               @events)))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest mouse-input-with-held-modifier-keys-simulates-modifier-click
  (let [events (atom [])
        stage (fx-sync
                (fn create-stage-for-modifier-click-input []
                  (let [target (doto (Rectangle. 100.0 100.0)
                                 (.setId "target")
                                 (.setFocusTraversable true))
                        root (group target)
                        stage (doto (Stage.)
                                (.setScene (Scene. root 100.0 100.0))
                                (.show)
                                (.requestFocus))]
                    (.addEventHandler target MouseEvent/MOUSE_CLICKED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj {:alt-down (.isAltDown event)
                                                              :control-down (.isControlDown event)
                                                              :shift-down (.isShiftDown event)
                                                              :meta-down (.isMetaDown event)}))))
                    (.requestFocus target)
                    stage)))]
    (try
      (let [target (plorer/one "#target")
            cases [{:name "alt-click" :key :alt :expected {:alt-down true :control-down false :shift-down false :meta-down false}}
                   {:name "ctrl-click" :key :control :expected {:alt-down false :control-down true :shift-down false :meta-down false}}
                   {:name "shift-click" :key :shift :expected {:alt-down false :control-down false :shift-down true :meta-down false}}
                   {:name "meta-click" :key :meta :expected {:alt-down false :control-down false :shift-down false :meta-down true}}
                   {:name "alt-click-via-keycode" :key KeyCode/ALT :expected {:alt-down true :control-down false :shift-down false :meta-down false}}]]
        (doseq [{:keys [name key expected]} cases]
          (testing name
            (reset! events [])
            (is (= target (plorer/key-press! stage key)))
            (is (= target (plorer/mouse-press! stage [50 50] :primary)))
            (is (= target (plorer/mouse-release! stage [50 50] :primary)))
            (is (= target (plorer/key-release! stage key)))
            (is (= [expected] @events)))))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest key-input-includes-held-modifier-flags
  (let [events (atom [])
        stage (fx-sync
                (fn create-stage-for-key-modifier-input []
                  (let [target (doto (Rectangle. 100.0 100.0)
                                 (.setId "target")
                                 (.setFocusTraversable true))
                        root (group target)
                        stage (doto (Stage.)
                                (.setScene (Scene. root 100.0 100.0))
                                (.show)
                                (.requestFocus))]
                    (.addEventHandler target KeyEvent/KEY_PRESSED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:pressed
                                                              (.getCode event)
                                                              (.isShiftDown event)]))))
                    (.addEventHandler target KeyEvent/KEY_TYPED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:typed
                                                              (.getCharacter event)
                                                              (.isShiftDown event)]))))
                    (.addEventHandler target KeyEvent/KEY_RELEASED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:released
                                                              (.getCode event)
                                                              (.isShiftDown event)]))))
                    (.requestFocus target)
                    stage)))]
    (try
      (let [target (plorer/one "#target")]
        (fx-sync (fn await-focus []
                   (is (= target (.getFocusOwner (.getScene target))))))
        (is (= target (plorer/key-press! stage :shift)))
        (is (= target (plorer/key-press! stage :a)))
        (is (= target (plorer/key-release! stage :a)))
        (is (= target (plorer/key-release! stage :shift)))
        (is (= target (plorer/key-press! stage :a)))
        (is (= target (plorer/key-release! stage :a)))
        (is (= [[:pressed KeyCode/SHIFT true]
                [:pressed KeyCode/A true]
                [:typed "A" true]
                [:released KeyCode/A true]
                [:released KeyCode/SHIFT false]
                [:pressed KeyCode/A false]
                [:typed "a" false]
                [:released KeyCode/A false]]
               @events)))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest key-input-dispatches-pressed-typed-and-released-to-focused-node
  (let [events (atom [])
        stage (fx-sync
                (fn create-stage-for-key-input []
                  (let [target (doto (Rectangle. 100.0 100.0)
                                 (.setId "target")
                                 (.setFocusTraversable true))
                        root (group target)
                        stage (doto (Stage.)
                                (.setScene (Scene. root 100.0 100.0))
                                (.show)
                                (.requestFocus))]
                    (.addEventHandler target KeyEvent/KEY_PRESSED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:pressed (.getCode event)]))))
                    (.addEventHandler target KeyEvent/KEY_TYPED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:typed (.getCharacter event)]))))
                    (.addEventHandler target KeyEvent/KEY_RELEASED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:released (.getCode event)]))))
                    (.requestFocus target)
                    stage)))]
    (try
      (let [target (plorer/one "#target")]
        (fx-sync (fn await-focus []
                   (is (= target (.getFocusOwner (.getScene target))))))
        (is (= target (plorer/key-press! stage :a)))
        (is (= target (plorer/key-release! stage :a)))
        (is (= [[:pressed KeyCode/A]
                [:typed "a"]
                [:released KeyCode/A]]
               @events)))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(defn- with-keyboard [f]
  (let [events (atom [])
        [stage field] (fx-sync
                        (fn create-keyboard-stage []
                          (let [field (TextField.)
                                root (VBox.)
                                scene (Scene. root 200.0 100.0)
                                stage (doto (Stage.) (.setScene scene))]
                            (.add (.getChildren root) field)
                            (.addEventFilter scene KeyEvent/ANY
                                             (reify EventHandler
                                               (handle [_ event]
                                                 (swap! events conj
                                                        [({KeyEvent/KEY_PRESSED :pressed
                                                           KeyEvent/KEY_TYPED :typed
                                                           KeyEvent/KEY_RELEASED :released} (.getEventType event))
                                                         (.getCode event) (.getText event) (.getCharacter event)
                                                         (cond-> #{}
                                                           (.isShiftDown event) (conj :shift)
                                                           (.isControlDown event) (conj :control)
                                                           (.isAltDown event) (conj :alt)
                                                           (.isMetaDown event) (conj :meta))]))))
                            (.show stage)
                            (.requestFocus field)
                            [stage field])))]
    (try
      (f stage field events)
      (finally
        (fx-sync (fn close-keyboard-stage [] (.close stage)))))))

(defn- tap-keys! [stage keys]
  (doseq [key keys]
    (plorer/key-press! stage key)
    (plorer/key-release! stage key)))

(deftest virtual-keyboard-matches-recorded-hello-events
  (with-keyboard
    (fn [stage field events]
      (tap-keys! stage [:h :e :l :l :o])
      (plorer/key-press! stage :shift)
      (tap-keys! stage [:digit1])
      (plorer/key-release! stage :shift)
      (is (= "hello!" (:text (plorer/props field :only [:text]))))
      (is (= (into (into []
                         (mapcat (fn [[code text]]
                                   [[:pressed code text KeyEvent/CHAR_UNDEFINED #{}]
                                    [:typed KeyCode/UNDEFINED "" text #{}]
                                    [:released code text KeyEvent/CHAR_UNDEFINED #{}]]))
                         [[KeyCode/H "h"] [KeyCode/E "e"] [KeyCode/L "l"] [KeyCode/L "l"] [KeyCode/O "o"]])
                   [[:pressed KeyCode/SHIFT "" KeyEvent/CHAR_UNDEFINED #{:shift}]
                    [:pressed KeyCode/DIGIT1 "!" KeyEvent/CHAR_UNDEFINED #{:shift}]
                    [:typed KeyCode/UNDEFINED "" "!" #{:shift}]
                    [:released KeyCode/DIGIT1 "!" KeyEvent/CHAR_UNDEFINED #{:shift}]
                    [:released KeyCode/SHIFT "" KeyEvent/CHAR_UNDEFINED #{}]])
             @events)))))

(deftest virtual-keyboard-us-layout
  (with-keyboard
    (fn [stage field _]
      (let [keys (concat (map (comp keyword str) "abcdefghijklmnopqrstuvwxyz")
                         [:back-quote :digit1 :digit2 :digit3 :digit4 :digit5 :digit6 :digit7 :digit8 :digit9 :digit0
                          :minus :equals :open-bracket :close-bracket :back-slash :semicolon :quote :comma :period :slash :space
                          :numpad0 :numpad1 :numpad2 :numpad3 :numpad4 :numpad5 :numpad6 :numpad7 :numpad8 :numpad9
                          :multiply :add :subtract :decimal :divide])]
        (tap-keys! stage keys)
        (is (= "abcdefghijklmnopqrstuvwxyz`1234567890-=[]\\;',./ 0123456789*+-./"
               (:text (plorer/props field :only [:text]))))
        (fx-sync (fn clear-field [] (.clear ^TextField field)))
        (plorer/key-press! stage :shift)
        (tap-keys! stage keys)
        (plorer/key-release! stage :shift)
        (is (= "ABCDEFGHIJKLMNOPQRSTUVWXYZ~!@#$%^&*()_+{}|:\"<>? 0123456789*+-./"
               (:text (plorer/props field :only [:text]))))))))

(deftest virtual-keyboard-caps-repeat-and-release-state
  (with-keyboard
    (fn [stage field events]
      (plorer/key-press! stage :caps)
      (plorer/key-press! stage :caps)
      (plorer/key-release! stage :caps)
      (tap-keys! stage [:a :digit1])
      (plorer/key-press! stage :shift)
      (tap-keys! stage [:b])
      (plorer/key-press! stage :digit1)
      (plorer/key-press! stage :digit1)
      (plorer/key-release! stage :shift)
      (plorer/key-release! stage :digit1)
      (is (= [:released KeyCode/DIGIT1 "!" KeyEvent/CHAR_UNDEFINED #{}] (last @events)))
      (tap-keys! stage [:caps :c])
      (is (= "A1b!!c" (:text (plorer/props field :only [:text]))))
      (with-keyboard
        (fn [other-stage other-field _]
          (tap-keys! stage [:caps])
          (plorer/key-press! stage :shift)
          (tap-keys! other-stage [:a :digit1])
          (is (= "a1" (:text (plorer/props other-field :only [:text]))))
          (plorer/key-release! stage :shift))))))

(deftest virtual-keyboard-non-text-keys-and-modifiers
  (with-keyboard
    (fn [stage field events]
      (tap-keys! stage [:down :up :left :right :home :end :f1 :escape :enter])
      (doseq [modifier [:control :alt :meta]]
        (plorer/key-press! stage modifier)
        (tap-keys! stage [:digit8])
        (plorer/key-release! stage modifier)
        (is (= #{} (last (last @events)))))
      (is (= "" (:text (plorer/props field :only [:text]))))
      (is (not-any? #(= :typed (first %)) @events))
      (tap-keys! stage [:a])
      (is (= "a" (:text (plorer/props field :only [:text])))))))

(deftest virtual-keyboard-is-independent-of-default-locale
  (let [previous-locale (Locale/getDefault)]
    (try
      (Locale/setDefault (Locale/forLanguageTag "tr-TR"))
      (is (= :id (#'plorer/bean-stem->prop-key "Id")))
      (with-keyboard
        (fn [stage field _]
          (tap-keys! stage [:i :digit1 :minus])
          (plorer/key-press! stage :shift)
          (tap-keys! stage [:i :digit1 :minus])
          (plorer/key-release! stage :shift)
          (is (= "i1-I!_" (:text (plorer/props field :only [:text]))))))
      (finally
        (Locale/setDefault previous-locale)))))

(deftest key-input-can-copy-selected-text-between-text-fields
  (let [shortcut-key (platform-shortcut-key)
        stage (fx-sync
                (fn create-stage-for-text-field-copy-paste []
                  (let [source (doto (TextField. "Hello")
                                 (.setId "source"))
                        target (doto (TextField.)
                                 (.setId "target"))
                        root (doto (VBox. 8.0)
                               (.setId "root"))
                        stage (doto (Stage.)
                                (.setScene (Scene. root 200.0 100.0))
                                (.show)
                                (.requestFocus))]
                    (.addAll (.getChildren root) [source target])
                    (.requestFocus source)
                    stage)))]
    (try
      (let [source (plorer/one "#source")
            target (plorer/one "#target")]
        (fx-sync (fn await-initial-focus []
                   (is (= source (.getFocusOwner (.getScene source))))))
        (is (= source (plorer/key-press! stage shortcut-key)))
        (is (= source (plorer/key-press! stage :a)))
        (is (= source (plorer/key-release! stage :a)))
        (is (= source (plorer/key-press! stage :c)))
        (is (= source (plorer/key-release! stage :c)))
        (is (= source (plorer/key-release! stage shortcut-key)))
        (is (= source (plorer/key-press! stage :tab)))
        (is (= target (plorer/key-release! stage :tab)))
        (fx-sync (fn await-tab-focus []
                   (is (= target (.getFocusOwner (.getScene target))))))
        (is (= target (plorer/key-press! stage shortcut-key)))
        (is (= target (plorer/key-press! stage :v)))
        (is (= target (plorer/key-release! stage :v)))
        (is (= target (plorer/key-release! stage shortcut-key)))
        (fx-sync (fn assert-text-values []
                   (is (= "Hello" (.getText ^TextField source)))
                   (is (= "Hello" (.getText ^TextField target))))))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest key-input-rejects-node-targets
  (let [stage (fx-sync
                (fn create-stage-for-key-focus-validation []
                  (let [left (doto (Rectangle. 100.0 100.0)
                               (.setId "left")
                               (.setFocusTraversable true))
                        right (doto (Rectangle. 100.0 100.0)
                                (.setId "right")
                                (.setFocusTraversable true))
                        root (group left right)
                        stage (doto (Stage.)
                                (.setScene (Scene. root 220.0 100.0))
                                (.show)
                                (.requestFocus))]
                    (.setTranslateX right 120.0)
                    (.requestFocus right)
                    stage)))]
    (try
      (let [left (plorer/one "#left")
            right (plorer/one "#right")]
        (fx-sync (fn await-focus []
                   (is (= right (.getFocusOwner (.getScene right))))))
        (is (thrown-with-msg? IllegalArgumentException #"Input target must be ROOT, a Window, or a Scene"
                              (plorer/key-press! left :enter))))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest key-input-accepts-scene-and-window
  (let [events (atom [])
        stage (fx-sync
                (fn create-stage-for-scene-and-window-key-input []
                  (let [target (doto (Rectangle. 100.0 100.0)
                                 (.setId "target")
                                 (.setFocusTraversable true))
                        root (group target)
                        stage (doto (Stage.)
                                (.setScene (Scene. root 100.0 100.0))
                                (.show)
                                (.requestFocus))]
                    (.addEventHandler target KeyEvent/KEY_PRESSED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:pressed (.getCode event)]))))
                    (.addEventHandler target KeyEvent/KEY_RELEASED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:released (.getCode event)]))))
                    (.requestFocus target)
                    stage)))]
    (try
      (let [scene (.getScene stage)
            target (plorer/one "#target")]
        (fx-sync (fn await-focus []
                   (is (= target (.getFocusOwner scene)))))
        (is (= target (plorer/key-press! scene :enter)))
        (is (= target (plorer/key-release! stage :enter)))
        (is (= [[:pressed KeyCode/ENTER]
                [:released KeyCode/ENTER]]
               @events)))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest key-input-defaults-to-root
  (let [events (atom [])
        stage (fx-sync
                (fn create-stage-for-root-key-input []
                  (let [target (doto (Rectangle. 100.0 100.0)
                                 (.setId "target")
                                 (.setFocusTraversable true))
                        root (group target)
                        stage (doto (Stage.)
                                (.setScene (Scene. root 100.0 100.0))
                                (.show)
                                (.requestFocus))]
                    (.addEventHandler target KeyEvent/KEY_PRESSED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:pressed (.getCode event)]))))
                    (.addEventHandler target KeyEvent/KEY_RELEASED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:released (.getCode event)]))))
                    (.requestFocus target)
                    stage)))]
    (try
      (let [target (plorer/one "#target")]
        (fx-sync (fn await-focus []
                   (is (= target (.getFocusOwner (.getScene target))))))
        (is (= target (plorer/key-press! :enter)))
        (is (= target (plorer/key-release! :enter)))
        (is (= [[:pressed KeyCode/ENTER]
                [:released KeyCode/ENTER]]
               @events)))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest key-input-accepts-keycode-enum-values
  (let [events (atom [])
        stage (fx-sync
                (fn create-stage-for-key-code-enum-input []
                  (let [target (doto (Rectangle. 100.0 100.0)
                                 (.setId "target")
                                 (.setFocusTraversable true))
                        root (group target)
                        stage (doto (Stage.)
                                (.setScene (Scene. root 100.0 100.0))
                                (.show)
                                (.requestFocus))]
                    (.addEventHandler target KeyEvent/KEY_PRESSED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:pressed (.getCode event)]))))
                    (.addEventHandler target KeyEvent/KEY_RELEASED
                                      (reify EventHandler
                                        (handle [_ event]
                                          (swap! events conj [:released (.getCode event)]))))
                    (.requestFocus target)
                    stage)))]
    (try
      (let [target (plorer/one "#target")]
        (fx-sync (fn await-focus []
                   (is (= target (.getFocusOwner (.getScene target))))))
        (is (= target (plorer/key-press! stage KeyCode/ENTER)))
        (is (= target (plorer/key-release! stage KeyCode/ENTER)))
        (is (= [[:pressed KeyCode/ENTER]
                [:released KeyCode/ENTER]]
               @events)))
      (finally
        (fx-sync (fn close-stage []
                   (.close stage)))))))

(deftest key-input-fails-when-there-is-no-focus-owner
  (let [target (Rectangle. 100.0 100.0)
        scene (Scene. (group target) 100.0 100.0)]
    (is (nil? (fx-sync (fn focus-owner []
                         (.getFocusOwner scene)))))
    (is (thrown-with-msg? IllegalStateException #"Key input requires a focused node"
                          (plorer/key-press! scene :enter)))))

(deftest mouse-input-uses-scene-coordinates-and-preserves-release-capture
  (let [events (atom [])
        [stage left right] (fx-sync
                             (fn []
                               (let [left (doto (Rectangle. 20.0 20.0)
                                            (.setTranslateX 30.0)
                                            (.setTranslateY 40.0)
                                            (.setScaleX 2.0)
                                            (.setScaleY 2.0))
                                     right (doto (Rectangle. 20.0 20.0)
                                             (.setTranslateX 120.0))
                                     stage (doto (Stage.)
                                             (.setScene (Scene. (group left right) 200.0 100.0))
                                             (.show))]
                                 (.addEventHandler left MouseEvent/MOUSE_PRESSED
                                                   (reify EventHandler
                                                     (handle [_ event]
                                                       (swap! events conj [(.getX event) (.getY event)
                                                                           (.getSceneX event) (.getSceneY event)
                                                                           (.getScreenX event) (.getScreenY event)]))))
                                 [stage left right])))]
    (try
      (let [scene (.getScene stage)
            screen-point (fx-sync #(.localToScreen left 10.0 10.0))]
        (is (= left (plorer/mouse-press! stage [40 50] :primary)))
        (is (= [[10.0 10.0 40.0 50.0 (.getX screen-point) (.getY screen-point)]] @events))
        (testing "release follows the press target, then the next press picks afresh"
          (is (= left (plorer/mouse-release! scene [130 10] :primary)))
          (is (= right (plorer/mouse-press! scene [130 10] :primary)))
          (is (= right (plorer/mouse-release! stage [-10 -10] :primary))))
        (testing "empty scene space is a valid target"
          (is (= scene (plorer/mouse-press! stage [190 90] :primary)))
          (is (= scene (plorer/mouse-release! stage [190 90] :primary)))))
      (finally
        (fx-sync #(.close stage))))))

(deftest input-validates-targets-and-coordinates
  (let [[stage scene node] (fx-sync
                             (fn []
                               (let [node (Rectangle. 100.0 100.0)
                                     scene (Scene. (group node) 100.0 100.0)]
                                 [(doto (Stage.) (.setScene scene) (.show)) scene node])))]
    (try
      (doseq [target [node nil :window]]
        (is (thrown-with-msg? IllegalArgumentException #"Input target must be ROOT, a Window, or a Scene"
                              (plorer/mouse-press! target [50 50] :primary)))
        (is (thrown-with-msg? IllegalArgumentException #"Input target must be ROOT, a Window, or a Scene"
                              (plorer/key-press! target :enter))))
      (doseq [position [nil [0] [0 0 0] '(0 0) [nil 0] [0 "0"] [Double/NaN 0] [0 Double/POSITIVE_INFINITY]]]
        (is (thrown-with-msg? IllegalArgumentException #"Mouse position must be a vector"
                              (plorer/mouse-press! stage position :primary))))
      (fx-sync #(.hide stage))
      (is (thrown-with-msg? IllegalStateException #"Mouse input requires a scene in a showing window"
                            (plorer/mouse-press! scene [50 50] :primary)))
      (finally
        (fx-sync #(.close stage)))))
  (let [stage (fx-sync #(Stage.))]
    (is (thrown-with-msg? IllegalStateException #"Input target requires a window with a scene"
                          (plorer/key-press! stage :enter))))
  (is (thrown-with-msg? IllegalStateException #"Mouse input requires a scene in a showing window"
                        (plorer/mouse-press! (Scene. (group)) [0 0] :primary))))

(deftest explicit-targets-work-with-multiple-open-windows
  (with-keyboard
    (fn [first-stage first-field first-events]
      (with-keyboard
        (fn [second-stage second-field second-events]
          (let [root (:el (plorer/tree :depth 0))
                first-scene (.getScene first-stage)
                second-scene (.getScene second-stage)]
            (is (thrown-with-msg? IllegalStateException #"exactly one open window, got 2"
                                  (plorer/key-press! :a)))
            (is (thrown-with-msg? IllegalStateException #"exactly one open window, got 2"
                                  (plorer/mouse-press! root [10 10] :primary)))
            (plorer/key-press! first-stage :shift)
            (tap-keys! second-scene [:b])
            (tap-keys! first-scene [:a])
            (plorer/key-release! first-scene :shift)
            (is (= "A" (:text (plorer/props first-field :only [:text]))))
            (is (= "b" (:text (plorer/props second-field :only [:text]))))
            (is (= 5 (count @first-events)))
            (is (= 3 (count @second-events)))
            (is (some? (plorer/mouse-press! first-stage [10 10] :primary)))
            (is (some? (plorer/mouse-release! first-scene [10 10] :primary)))))))))

(deftest point-converts-relative-layout-bounds-through-transforms
  (let [node (fx-sync
               (fn []
                 (let [node (doto (Rectangle. 10.0 20.0 80.0 40.0)
                              (.setScaleX 2.0)
                              (.setScaleY 3.0))
                       parent (doto (group node)
                                (.setTranslateX 100.0)
                                (.setTranslateY 200.0))]
                   (Scene. parent)
                   node)))]
    (is (= [70.0 180.0] (plorer/point node 0 0)))
    (is (= [150.0 240.0] (plorer/point node 0.5 0.5)))
    (is (= [230.0 300.0] (plorer/point node 1 1)))
    (is (= [110.0 270.0] (plorer/point node 1/4 3/4)))
    (testing "also works when called on the JavaFX thread"
      (is (= [150.0 240.0] (fx-sync #(plorer/point node 0.5 0.5)))))
    (testing "positions follow the node's local axes after rotation"
      (fx-sync #(.setRotate node 90.0))
      (let [[x y] (plorer/point node 1/4 3/4)]
        (is (< (abs (- x 120.0)) 1e-9))
        (is (< (abs (- y 200.0)) 1e-9))))))

(deftest point-inside-nested-subscenes-composes-with-mouse-input
  (let [[stage node] (fx-sync
                       (fn []
                         (let [node (doto (Rectangle. 10.0 20.0)
                                      (.setTranslateX 5.0)
                                      (.setTranslateY 7.0))
                               inner (doto (SubScene. (group node) 100.0 100.0)
                                       (.setTranslateX 20.0)
                                       (.setTranslateY 30.0))
                               outer (doto (SubScene. (group inner) 200.0 200.0)
                                       (.setTranslateX 100.0)
                                       (.setTranslateY 200.0))
                               stage (doto (Stage.)
                                       (.setScene (Scene. (group outer) 500.0 500.0))
                                       (.show))]
                           [stage node])))]
    (try
      (let [position (plorer/point node 0.5 0.5)]
        (is (= [130.0 247.0] position))
        (is (= node (plorer/mouse-press! stage position :primary)))
        (is (= node (plorer/mouse-release! stage position :primary))))
      (finally
        (fx-sync #(.close stage))))))

(deftest point-validates-node-and-relative-coordinates
  (let [node (Rectangle. 10.0 10.0)]
    (is (thrown-with-msg? IllegalStateException #"Point requires a node with a scene"
                          (plorer/point node 0.5 0.5)))
    (let [scene (Scene. (group node))]
      (doseq [target [nil scene]]
        (is (thrown-with-msg? IllegalArgumentException #"Point requires a Node"
                              (plorer/point target 0.5 0.5))))
      (doseq [[x y] [[-0.1 0] [0 1.1] [nil 0] [0 "1"] [Double/NaN 0] [0 Double/POSITIVE_INFINITY]]]
        (is (thrown-with-msg? IllegalArgumentException #"Point coordinates must be numbers from 0 to 1"
                              (plorer/point node x y)))))))

(deftest key-tap-types-and-releases-with-default-and-explicit-targets
  (with-keyboard
    (fn [stage field events]
      (doseq [tap [#(plorer/key-tap! :a)
                   #(plorer/key-tap! stage KeyCode/A)
                   #(plorer/key-tap! (.getScene stage) :a)]]
        (reset! events [])
        (is (= field (tap)))
        (is (= [[:pressed KeyCode/A "a" KeyEvent/CHAR_UNDEFINED #{}]
                [:typed KeyCode/UNDEFINED "" "a" #{}]
                [:released KeyCode/A "a" KeyEvent/CHAR_UNDEFINED #{}]]
               @events)))
      (is (= "aaa" (:text (plorer/props field :only [:text])))))))

(deftest key-chord-orders-events-and-releases-modifiers
  (with-keyboard
    (fn [stage field events]
      (plorer/key-chord! [:shift :digit1])
      (is (= "!" (:text (plorer/props field :only [:text]))))
      (is (= [[:pressed KeyCode/SHIFT #{:shift}]
              [:pressed KeyCode/DIGIT1 #{:shift}]
              [:typed KeyCode/UNDEFINED #{:shift}]
              [:released KeyCode/DIGIT1 #{:shift}]
              [:released KeyCode/SHIFT #{}]]
             (mapv (fn [[type code _ _ modifiers]] [type code modifiers]) @events)))
      (doseq [target [stage (.getScene stage)]]
        (reset! events [])
        (plorer/key-chord! target [KeyCode/CONTROL :shift :z])
        (is (= [[:pressed KeyCode/CONTROL #{:control}]
                [:pressed KeyCode/SHIFT #{:control :shift}]
                [:pressed KeyCode/Z #{:control :shift}]
                [:released KeyCode/Z #{:control :shift}]
                [:released KeyCode/SHIFT #{:control}]
                [:released KeyCode/CONTROL #{}]]
               (mapv (fn [[type code _ _ modifiers]] [type code modifiers]) @events))))
      (plorer/key-tap! stage :a)
      (is (= "!a" (:text (plorer/props field :only [:text])))))))

(deftest mouse-click-generates-clicks-and-releases-between-targets
  (let [events (atom [])
        [stage left right] (fx-sync
                             (fn []
                               (let [left (Rectangle. 50.0 50.0)
                                     right (doto (Rectangle. 50.0 50.0) (.setTranslateX 100.0))
                                     root (group left right)
                                     stage (doto (Stage.)
                                             (.setScene (Scene. root 150.0 50.0))
                                             (.show))]
                                 (.addEventHandler root MouseEvent/ANY
                                                   (reify EventHandler
                                                     (handle [_ event]
                                                       (when-let [type ({MouseEvent/MOUSE_PRESSED :pressed
                                                                         MouseEvent/MOUSE_RELEASED :released
                                                                         MouseEvent/MOUSE_CLICKED :clicked} (.getEventType event))]
                                                         (swap! events conj [type (.getTarget event) (.isPrimaryButtonDown event)])))))
                                 [stage left right])))]
    (try
      (is (= left (plorer/mouse-click! (plorer/point left 0.5 0.5) :primary)))
      (is (= right (plorer/mouse-click! stage (plorer/point right 0.5 0.5) MouseButton/PRIMARY)))
      (is (= left (plorer/mouse-click! (.getScene stage) (plorer/point left 0.5 0.5) :primary)))
      (is (= [[:pressed left true] [:released left false] [:clicked left false]
              [:pressed right true] [:released right false] [:clicked right false]
              [:pressed left true] [:released left false] [:clicked left false]]
             @events))
      (finally
        (fx-sync #(.close stage))))))

(defn test-ns-hook []
  (start-fx-runtime!)
  (Platform/setImplicitExit false)
  (try
    (t/test-vars
      (sort-by (comp :line meta)
               (filter (comp :test meta)
                       (vals (ns-interns 'cljfx.plorer-test)))))
    (finally
      (Platform/exit))))
