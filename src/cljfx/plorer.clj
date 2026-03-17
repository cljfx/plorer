(ns cljfx.plorer
  (:require [clojure.string :as str])
  (:import [java.beans Introspector]
           [java.lang.reflect Method Modifier]
           [java.util ArrayDeque]
           [javafx.application Platform]
           [javafx.beans.value ObservableValue]
           [javafx.collections ObservableList]
           [javafx.scene Parent Scene SubScene]
           [javafx.stage Window]))

(set! *warn-on-reflection* true)

;; region util

(defmacro on-ui-thread [& body]
  `(if (Platform/isFxApplicationThread)
     (do ~@body)
     (let [result# (promise)]
       (Platform/runLater (bound-fn [] (deliver result# (try [::ok (do ~@body)] (catch Throwable t# [::err t#])))))
       (let [[status# value#] @result#]
         (case status# ::ok value# ::err (throw value#))))))

;; endregion

;; region props

(defn- bean-stem->prop-key [stem]
  (->> (re-seq #"[A-Z]+(?=$|[A-Z][a-z0-9])|[A-Z]?[a-z0-9]+" stem)
       (map str/lower-case)
       (str/join "-")
       keyword))

(def ^:private class-prop-getters
  (memoize
    (fn [class]
      (reduce
        (fn [prop-getters ^Method method]
          (let [method-name (.getName method)]
            (cond
              (or (not (zero? (.getParameterCount method)))
                  (Modifier/isStatic (.getModifiers method)))
              prop-getters

              (and (str/ends-with? method-name "Property")
                   (pos? (- (count method-name) (count "Property")))
                   (.isAssignableFrom ObservableValue (.getReturnType method)))
              (let [prop-key (bean-stem->prop-key
                               (subs method-name 0 (- (count method-name) (count "Property"))))]
                (assoc prop-getters prop-key
                       (fn [el]
                         (when-some [^ObservableValue value
                                     (.invoke method el (object-array 0))]
                           (.getValue value)))))

              (and (str/starts-with? method-name "get")
                   (> (count method-name) (count "get"))
                   (.isAssignableFrom ObservableList (.getReturnType method)))
              (let [prop-key (bean-stem->prop-key
                               (Introspector/decapitalize (subs method-name (count "get"))))]
                (assoc prop-getters prop-key
                       (fn [el]
                         (.invoke method el (object-array 0)))))

              :else
              prop-getters)))
        {}
        (.getMethods ^Class class)))))

(defn props
  "Return a map of supported props to current values for an element.
  Pass :only with a collection of prop keys to limit reads. Unsupported
  keys are omitted; supported keys with nil values are included."
  [el & {:keys [only]}]
  (on-ui-thread
    (let [prop-getters (class-prop-getters (class el))]
      (reduce
        (fn [m prop-key]
          (if-some [getter (get prop-getters prop-key)]
            (assoc m prop-key (getter el))
            m))
        {}
        (or only (keys prop-getters))))))

;; endregion

;; region tree

(defprotocol ^:private ChildLookup (-children [el]))

(extend-protocol ChildLookup
  Window (-children [window] (cond-> [] (.getScene window) (conj (.getScene window))))
  Scene (-children [scene] (cond-> [] (.getRoot scene) (conj (.getRoot scene))))
  Parent (-children [parent] (vec (.getChildrenUnmodifiable parent)))
  SubScene (-children [sub-scene] (cond-> [] (.getRoot sub-scene) (conj (.getRoot sub-scene))))
  Object (-children [_] []))

(defn tree
  "Return a tree rooted at el. Pass :depth to limit recursion and :props
  to include selected props at each node."
  [el & {:keys [depth] prop-keys :props}]
  (on-ui-thread
    (let [next-depth (when (some? depth) (dec depth))]
      (cond-> {:el el}
        prop-keys
        (assoc :props (props el :only prop-keys))

        (not (and (some? depth) (zero? depth)))
        (assoc :children (mapv #(tree % :depth next-depth :props prop-keys) (-children el)))))))

;; endregion

;; region query

(def ^:private ROOT (reify ChildLookup (-children [_] (vec (Window/getWindows)))))

(defn- query-descendants [el]
  (let [stack (ArrayDeque.)]
    (loop [result (transient [])
           children (-children el)]
      (doseq [child (rseq (vec children))]
        (.push stack child))
      (if (.isEmpty stack)
        (persistent! result)
        (let [node (.pop stack)]
          (recur (conj! result node) (-children node)))))))

(defn- parse-string-selector [selector]
  (let [parts (str/split selector #"\." -1)]
    (cond
      (and (str/starts-with? selector "#") (every? seq parts))
      (cond-> {:id (subs (first parts) 1)} (next parts) (assoc :fx.plorer/style-classes (set (next parts))))

      (and (str/starts-with? selector ".") (empty? (first parts)) (every? seq (rest parts)))
      {:fx.plorer/style-classes (set (rest parts))}

      :else
      (throw (IllegalArgumentException. (str "Unsupported string selector: " selector))))))

(defn- canonicalize-selector [selector]
  (cond
    (map? selector) selector
    (instance? Class selector) {:fx.plorer/class selector}
    (= selector *) {}
    (string? selector) (parse-string-selector selector)
    (ifn? selector) {:fx.plorer/pred selector}
    :else (throw (IllegalArgumentException. (str "Unsupported selector: " selector)))))

(defn- matcher [selector]
  (let [selector (canonicalize-selector selector)
        prop-selector (dissoc selector :fx.plorer/class :fx.plorer/pred :fx.plorer/style-classes)
        pred (:fx.plorer/pred selector)
        class (:fx.plorer/class selector)
        style-classes (:fx.plorer/style-classes selector)
        prop-keys (cond-> (reduce-kv
                            (fn [prop-keys prop-key _]
                              (conj prop-keys prop-key))
                            []
                            prop-selector)
                    style-classes (conj :style-class))
        read-props (if (seq prop-keys)
                     (fn [el]
                       (props el :only prop-keys))
                     (constantly nil))
        preds (cond-> []
                class
                (conj #(instance? class %))

                pred
                (conj #(boolean (pred %)))

                (seq prop-keys)
                (conj (let [prop-preds
                            (cond-> (reduce-kv
                                      (fn [prop-checks prop-key expected]
                                        (conj prop-checks
                                              (if (or (fn? expected) (var? expected))
                                                (fn [prop-values]
                                                  (and (contains? prop-values prop-key)
                                                       (boolean (expected (get prop-values prop-key)))))
                                                (fn [prop-values]
                                                  (and (contains? prop-values prop-key)
                                                       (= expected (get prop-values prop-key)))))))
                                      []
                                      prop-selector)
                              style-classes
                              (conj (fn [prop-values]
                                      (let [actual-style-classes (:style-class prop-values)]
                                        (and (contains? prop-values :style-class)
                                             (every? (set actual-style-classes) style-classes))))))]
                        (case (count prop-preds)
                          0 any?
                          1 (let [prop-pred (prop-preds 0)]
                              (fn [el]
                                (prop-pred (read-props el))))
                          (let [prop-pred (apply every-pred prop-preds)]
                            (fn [el]
                              (prop-pred (read-props el))))))))]
    (case (count preds)
      0 any?
      1 (preds 0)
      (apply every-pred preds))))

(defn- normalize-query-steps [selectors]
  (loop [selectors selectors
         direct false
         steps []]
    (if-some [selector (first selectors)]
      (if (= selector >)
        (do
          (when (empty? (rest selectors))
            (throw (IllegalArgumentException. "Query selector cannot end with >")))
          (recur (rest selectors) true steps))
        (recur (rest selectors)
               false
               (conj steps {:direct direct :match (matcher selector)})))
      steps)))

(defn- execute-query [selectors]
  (reduce
    (fn [els {:keys [direct match]}]
      (into []
            (comp
              (mapcat #(if direct (-children %) (query-descendants %)))
              (filter match)
              (distinct))
            els))
    [ROOT]
    (normalize-query-steps selectors)))

(defn all [& selectors]
  (on-ui-thread
    (execute-query selectors)))

(defn one [& selectors]
  (on-ui-thread
    (let [results (execute-query selectors)]
      (if (= 1 (count results))
        (first results)
        (throw (IllegalStateException.
                 (str "Expected exactly one match, got " (count results))))))))

;; endregion
