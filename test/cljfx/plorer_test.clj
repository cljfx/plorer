(ns cljfx.plorer-test
  (:require [cljfx.plorer :as plorer]
            [clojure.test :refer [deftest is testing]])
  (:import [javafx.scene.text Text]))

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
