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
  (tree :depth 3)
  (tree el :props [:id])
  (all Text)
  (one \"#root\")
  (key-press! el :alt)
  (mouse-press! el :primary)
  (mouse-release! el :primary)
  (key-release! el :alt)
  ```"
  (:require [clojure.string :as str])
  (:import [com.sun.javafx.scene SceneHelper]
           [java.beans Introspector]
           [java.lang.reflect Method Modifier]
           [java.util ArrayDeque WeakHashMap]
           [javafx.application Platform]
           [javafx.beans.value ObservableValue]
           [javafx.collections ObservableList]
           [javafx.event EventHandler]
           [javafx.geometry Bounds Point2D]
           [javafx.scene Node Parent Scene SubScene]
           [javafx.scene.input KeyCode KeyEvent MouseButton MouseEvent PickResult]
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
  "Return a map of supported property values for `el`.

  Behavior:
  - returned keys are the supported logical props for `el`
  - unsupported keys are omitted
  - supported keys with nil values are included
  - `:only` limits which keys are read

  Example:

  ```clojure
  (props el)
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

(def ^:private ROOT
  (reify
    Object
    (toString [_] "cljfx.plorer/ROOT")
    ChildLookup
    (-children [_] (vec (Window/getWindows)))))

(defn ^{:arglists '([el? & {:keys [depth props]}])} tree
  "Return a tree for `el`, or from the synthetic root when `el` is omitted.

  Behavior:
  - with no `el`, traversal starts from the synthetic root
  - each node always includes `:el`
  - `:props` is included only when requested
  - `:children` is included unless `:depth` is 0
  - `:depth 0` returns just the current node
  - `:depth 1` includes immediate children

  Examples:

  ```clojure
  (tree el)
  (tree el :props [:id])
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
    (or (fn? selector) (var? selector)) {:fx.plorer/pred selector}
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
  - `:fx.plorer/class` adds a class match
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
      (throw (IllegalStateException. (str "Expected exactly one match, got " (count results)))))))

;; endregion

;; region key input

(defn- resolve-input-target [el]
  (condp instance? el
    Node (let [scene (.getScene ^Node el)]
           (when (nil? scene)
             (throw (IllegalStateException. "Input target requires a node with a scene")))
           {:scene scene
            :node el})
    Scene (let [root (.getRoot ^Scene el)]
            (when (nil? root)
              (throw (IllegalStateException. "Input target requires a scene root")))
            {:scene el
             :node root})
    Window (let [scene (.getScene ^Window el)]
             (when (nil? scene)
               (throw (IllegalStateException. "Input target requires a window with a scene")))
             (recur scene))
    (throw (IllegalArgumentException. "Input target must be a Window, Scene, or Node"))))

(defn- enum-keyword->name [k]
  (-> (name k) (str/replace "-" "_") str/upper-case))

(defn- node-descendant? [ancestor node]
  (loop [node node]
    (cond
      (nil? node) false
      (identical? ancestor node) true
      :else (recur (.getParent ^Node node)))))

(def ^:private modifier-key-codes #{KeyCode/ALT KeyCode/CONTROL KeyCode/META KeyCode/SHIFT})

(def ^:private ^WeakHashMap scene->held-modifier-codes (WeakHashMap.))

(defn- normalize-key-code [key]
  (cond
    (instance? KeyCode key) key
    (keyword? key) (KeyCode/valueOf (enum-keyword->name key))
    :else (throw (IllegalArgumentException. (str "Unsupported key: " key)))))

(defn- held-modifier-codes [^Scene scene]
  (or (.get scene->held-modifier-codes scene) #{}))

(defn- update-held-modifier-codes!
  [^Scene scene f & args]
  (let [held-modifier-codes (apply f (held-modifier-codes scene) args)]
    (.put scene->held-modifier-codes scene held-modifier-codes)
    held-modifier-codes))

(defn- key-event [event-type typed-character ^KeyCode code held-modifier-codes]
  (KeyEvent.
    #_event-type event-type
    #_character (if (= KeyEvent/KEY_TYPED event-type) typed-character KeyEvent/CHAR_UNDEFINED)
    #_text (if (= KeyEvent/KEY_TYPED event-type) "" (.getName code))
    #_code (if (= KeyEvent/KEY_TYPED event-type) KeyCode/UNDEFINED code)
    #_shift-down (contains? held-modifier-codes KeyCode/SHIFT)
    #_control-down (contains? held-modifier-codes KeyCode/CONTROL)
    #_alt-down (contains? held-modifier-codes KeyCode/ALT)
    #_meta-down (contains? held-modifier-codes KeyCode/META)))

(defn- dispatch-key! [el event-type key]
  (on-ui-thread
    (let [{:keys [^Scene scene node]} (resolve-input-target el)
          focus-owner (.getFocusOwner ^Scene scene)
          code (normalize-key-code key)
          character (.getChar ^KeyCode code)
          typed-character (when (and (= 1 (count character)) (<= (int \space) (int (first character))))
                            character)
          held-modifier-codes (cond
                                (and (= KeyEvent/KEY_PRESSED event-type)
                                     (modifier-key-codes code))
                                (update-held-modifier-codes! scene conj code)

                                :else
                                (held-modifier-codes scene))]
      (when (nil? focus-owner)
        (throw (IllegalStateException. "Key input requires a focused node")))
      (when-not (node-descendant? node focus-owner)
        (throw (IllegalArgumentException. "Key input target does not contain the focused node")))
      (try
        (SceneHelper/processKeyEvent scene (key-event event-type typed-character code held-modifier-codes))
        (when (and (= KeyEvent/KEY_PRESSED event-type) typed-character)
          (SceneHelper/processKeyEvent scene (key-event KeyEvent/KEY_TYPED typed-character code held-modifier-codes)))
        (finally
          (when (and (= KeyEvent/KEY_RELEASED event-type) (modifier-key-codes code))
            (update-held-modifier-codes! scene disj code))))
      focus-owner)))

(defn key-press!
  "Dispatch a synthetic key press for `key` on `el`.

  `key` may be a `KeyCode` or a keyword such as `:enter`, `:tab`, `:escape`,
  `:a`, `:shift`, `:control`, `:alt`, or `:meta`.

  Returns the focused node that received the event. Printable keys also emit a
  synthetic `KEY_TYPED` event. Pair each call with `key-release!`, especially
  for modifiers, to avoid leaking held-key state. Throws when there is no focus
  owner or when the focused node is outside `el`."
  [el key]
  (dispatch-key! el KeyEvent/KEY_PRESSED key))

(defn key-release!
  "Dispatch a synthetic key release for `key` on `el`.

  Accepts the same key forms as `key-press!` and returns the focused node that
  received the event.

  Modifier state is tracked across `key-press!` and `key-release!`, so release
  keys you press, e.g. `:shift`, `:control`, `:alt`, or `:meta`. Throws when
  there is no focus owner or when the focused node is outside `el`."
  [el key]
  (dispatch-key! el KeyEvent/KEY_RELEASED key))

;; endregion

;; region mouse input

(defn- mouse-event [event-type ^Point2D scene-point ^Point2D screen-point ^MouseButton button held-modifier-codes]
  (MouseEvent.
    #_event-type event-type
    #_x (.getX scene-point)
    #_y (.getY scene-point)
    #_screen-x (.getX screen-point)
    #_screen-y (.getY screen-point)
    #_button button
    #_click-count 1
    #_shift-down (contains? held-modifier-codes KeyCode/SHIFT)
    #_control-down (contains? held-modifier-codes KeyCode/CONTROL)
    #_alt-down (contains? held-modifier-codes KeyCode/ALT)
    #_meta-down (contains? held-modifier-codes KeyCode/META)
    #_primary-button-down (identical? MouseButton/PRIMARY button)
    #_middle-button-down (identical? MouseButton/MIDDLE button)
    #_secondary-button-down (identical? MouseButton/SECONDARY button)
    #_synthesized false
    #_popup-trigger false
    #_still-since-press false
    #_pick-result (PickResult. nil (.getX scene-point) (.getY scene-point))))

(defn- dispatch-mouse! [el event-type button]
  (on-ui-thread
    (let [{:keys [^Scene scene ^Node node]} (resolve-input-target el)
          bounds (.getLayoutBounds node)
          center-of-node (Point2D. (+ (.getMinX ^Bounds bounds) (/ (.getWidth ^Bounds bounds) 2.0))
                                   (+ (.getMinY ^Bounds bounds) (/ (.getHeight ^Bounds bounds) 2.0)))
          scene-point (or (.localToScene node center-of-node)
                          (throw (IllegalStateException. "Mouse input requires a node with scene coordinates")))
          screen-point (or (.localToScreen node center-of-node)
                           (throw (IllegalStateException. "Mouse input requires a showing node")))
          button (cond
                   (instance? MouseButton button) button
                   (keyword? button) (MouseButton/valueOf (enum-keyword->name button))
                   :else (throw (IllegalArgumentException. (str "Unsupported mouse button: " button))))
          held-modifier-codes (held-modifier-codes scene)
          event (mouse-event event-type scene-point screen-point button held-modifier-codes)
          captured-target (atom nil)
          handler (reify EventHandler
                    (handle [_ event]
                      (swap! captured-target #(or % (.getTarget event)))))]
      (.addEventFilter scene event-type handler)
      (try
        (SceneHelper/processMouseEvent scene event)
        (let [actual @captured-target]
          (when-not (and (instance? Node actual) (node-descendant? node actual))
            (when (= MouseEvent/MOUSE_PRESSED event-type)
              (SceneHelper/processMouseEvent scene (mouse-event MouseEvent/MOUSE_RELEASED scene-point screen-point button held-modifier-codes)))
            (throw (ex-info "Mouse interaction resolved to a different element" {:el actual})))
          actual)
        (finally
          (.removeEventFilter scene event-type handler))))))

(defn mouse-press!
  "Dispatch a synthetic mouse press for `button` on `el`.

  `button` may be a `MouseButton` or one of `:primary`, `:middle`, or
  `:secondary`.

  Clicks the visual center of `el` and returns the picked node. Throws if `el`
  is not showing, has no scene coordinates, or if a different node is picked.
  Pair each call with `mouse-release!`; a failed press sends a balancing
  release first."
  [el button]
  (dispatch-mouse! el MouseEvent/MOUSE_PRESSED button))

(defn mouse-release!
  "Dispatch a synthetic mouse release for `button` on `el`.

  Accepts the same button forms as `mouse-press!` and returns the picked node.
  Supported button keywords are `:primary`, `:middle`, and `:secondary`.

  The event uses the visual center of `el`. Throws if `el` is not showing, has
  no scene coordinates, or if a different node is picked."
  [el button]
  (dispatch-mouse! el MouseEvent/MOUSE_RELEASED button))

;; endregion
