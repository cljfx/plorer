(ns cljfx.plorer
  "Inspect and query JavaFX scene graphs from Clojure.
   
  `el` is a scene graph element, e.g., Node, Window, or Scene.

  Public entry points:
  - `props` for reading supported properties from an el
  - `tree` for building a nested representation of an el and its children
  - `all` and `one` for querying the live JavaFX graph

  Examples:

  ```clojure
  (props el :only [:id :text])
  (tree)
  (tree el :props [:id])
  (all Text)
  (one \"#root\")
  ```"
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

(defmacro ^:private on-ui-thread [& body]
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
    (fn class->prop-getters [class]
      (reduce
        (fn collect-method-prop-getters [prop-getters ^Method method]
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
                       (fn read-observable-property [el]
                         (when-some [^ObservableValue value
                                     (.invoke method el (object-array 0))]
                           (.getValue value)))))

              (and (str/starts-with? method-name "get")
                   (> (count method-name) (count "get"))
                   (.isAssignableFrom ObservableList (.getReturnType method)))
              (let [prop-key (bean-stem->prop-key
                               (Introspector/decapitalize (subs method-name (count "get"))))]
                (assoc prop-getters prop-key
                       (fn read-observable-list-property [el]
                         (.invoke method el (object-array 0)))))

              :else
              prop-getters)))
        {}
        (.getMethods ^Class class)))))

(defn props
  "Return supported property values for `el`.

  Use `:only` to limit keys. Unsupported keys are omitted; nil values are kept.

  Example:

  ```clojure
  (props text-el :only [:text :id])
  ```"
  [el & {:keys [only]}]
  (on-ui-thread
    (let [prop-getters (class-prop-getters (class el))]
      (reduce
        (fn assoc-supported-prop [m prop-key]
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

(def ^:private ROOT (reify ChildLookup (-children [_] (vec (Window/getWindows)))))

(defn ^{:arglists '([el? & {:keys [depth props]}])} tree
  "Return a tree rooted at `el`.

  With no `el`, uses all open windows.
  Use `:depth` to limit recursion and `:props` to include selected props.

  Examples:

  ```clojure
  (tree node :props [:id])
  (tree :depth 3 :props [:id :title])
  ```"
  [& args]
  (let [[el options] (if (even? (count args))
                       [ROOT args]
                       [(first args) (next args)])
        {:keys [depth] prop-keys :props} (apply hash-map options)]
    (on-ui-thread
      (let [next-depth (when (some? depth) (dec depth))]
        (cond-> {:el el}
          prop-keys
          (assoc :props (props el :only prop-keys))

          (not (and (some? depth) (zero? depth)))
          (assoc :children (mapv (fn child->tree [child]
                                   (tree child :depth next-depth :props prop-keys))
                                 (-children el))))))))

;; endregion

;; region query

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
                            (fn collect-prop-key [prop-keys prop-key _]
                              (conj prop-keys prop-key))
                            []
                            prop-selector)
                    style-classes (conj :style-class))
        read-props (if (seq prop-keys)
                     (fn read-selected-props [el]
                       (props el :only prop-keys))
                     (constantly nil))
        preds (cond-> []
                class
                (conj (fn matches-class? [el]
                        (instance? class el)))

                pred
                (conj (fn matches-selector-pred? [el]
                        (boolean (pred el))))

                (seq prop-keys)
                (conj (let [prop-preds
                            (cond-> (reduce-kv
                                      (fn build-prop-check [prop-checks prop-key expected]
                                        (conj prop-checks
                                              (if (or (fn? expected) (var? expected))
                                                (fn matches-prop-predicate? [prop-values]
                                                  (and (contains? prop-values prop-key)
                                                       (boolean (expected (get prop-values prop-key)))))
                                                (fn matches-prop-value? [prop-values]
                                                  (and (contains? prop-values prop-key)
                                                       (= expected (get prop-values prop-key)))))))
                                      []
                                      prop-selector)
                              style-classes
                              (conj (fn has-style-classes? [prop-values]
                                      (let [actual-style-classes (:style-class prop-values)]
                                        (and (contains? prop-values :style-class)
                                             (every? (set actual-style-classes) style-classes))))))]
                        (if (= 1 (count prop-preds))
                          (let [prop-pred (prop-preds 0)]
                            (fn matches-single-prop-pred? [el]
                              (prop-pred (read-props el))))
                          (let [prop-pred (apply every-pred prop-preds)]
                            (fn matches-all-prop-preds? [el]
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
    (fn execute-step [els {:keys [direct match]}]
      (into []
            (comp
              (mapcat (fn step-children-or-descendants [el]
                        (if direct (-children el) (query-descendants el))))
              (filter match)
              (distinct))
            els))
    [ROOT]
    (normalize-query-steps selectors)))

(defn all
  "Return all matching els for `selectors`.

  Queries start from all open windows and run left to right. `>` makes the
  next step direct-child only.

  Selector forms:
  - `Class` matches by `instance?`
  - `*` matches anything
  - strings match `#id`, `.style-class`, or `#id.style-class`
  - functions and vars that resolve to functions are predicates
  - maps match el properties (equality or predicate)

  Map keys:
  - ordinary keys compare against el properties
  - `:fx.plorer/class` adds a type match
  - `:fx.plorer/pred` adds an el predicate
  - `:fx.plorer/style-classes` requires all listed CSS classes

  Example:

  ```clojure
  (all > Window)
  (all VBox > Text)
  (all {:id some?})
  (all \"#id.class.other-class\")
  (all {:fx.plorer/class Text :id \"title\" :fx.plorer/style-classes #{\"primary\"}})
  ```"
  [& selectors]
  (on-ui-thread
    (execute-query selectors)))

(defn one
  "Return the only matching el for `selectors`.

  Same selector forms as `all`. Throws `IllegalStateException` on 0 or many
  matches.

  Example:

  ```clojure
  (one \"#root\")
  ```"
  [& selectors]
  (let [results (apply all selectors)]
    (if (= 1 (count results))
      (first results)
      (throw (IllegalStateException.
               (str "Expected exactly one match, got " (count results)))))))

;; endregion
