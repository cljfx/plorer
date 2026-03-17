(ns cljfx.plorer-test
  (:require [cljfx.plorer :as plorer]
            [clojure.test :as t :refer [deftest is testing]])
  (:import [javafx.application Platform]
           [javafx.scene Group Scene SubScene]
           [javafx.scene.text Text]
           [javafx.stage Stage]))

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

(defn test-ns-hook []
  (start-fx-runtime!)
  (try
    (t/test-vars
      (sort-by (comp :line meta)
               (filter (comp :test meta)
                       (vals (ns-interns 'cljfx.plorer-test)))))
    (finally
      (Platform/exit))))
