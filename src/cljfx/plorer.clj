(ns cljfx.plorer
  "Explore and drive a running JavaFX application from its Clojure REPL.

  Inspection returns live JavaFX objects. Start at a node, scene, or window;
  omitting the starting element inspects all open windows.

  Input targets a scene or window; omitting the target requires exactly one
  open window. Mouse positions use scene coordinates, and keyboard input goes
  to the scene's focused node. The desktop pointer is not moved.
  Mouse input requires a showing window.

  Call functions directly from the REPL; thread scheduling is handled for you.
  Calls finish synchronously, but application work they trigger may finish later.

  Example with one open window (p is the alias used in these docstrings):
    (require '[cljfx.plorer :as p])

    ;; Inspect the window and find a text field by an ID from the tree.
    (def window (p/one javafx.stage.Window))
    (p/tree window :depth 3 :props [:id :text])
    (def field (p/one window \"#name\")) ; replace with your field's ID

    ;; Click the field's center and type hi!.
    (p/mouse-click! window (p/point field 0.5 0.5) :primary)
    (doseq [key [:h :i]] (p/key-tap! window key))
    (p/key-chord! window [:shift :digit1])

    ;; Read the result from the live object.
    (p/props field :only [:text])

    ;; Hover over a scrollable list and scroll down.
    (def items (p/one window \"#items\")) ; replace with your list's ID
    (p/mouse-move! window (p/point items 0.5 0.5))
    (p/scroll! window (p/point items 0.5 0.5) 0 -100)

    ;; Take a screenshot.
    (p/screenshot! window)"
  (:require [clojure.string :as str])
  (:import [com.sun.javafx.scene SceneHelper]
           [com.sun.javafx.tk TKSceneListener]
           [java.awt.image BufferedImage]
           [java.beans Introspector]
           [java.io File IOException]
           [java.lang.reflect Constructor Method Modifier]
           [java.util ArrayDeque Locale WeakHashMap]
           [javafx.application Platform]
           [javafx.beans.value ObservableValue]
           [javafx.collections ObservableList]
           [javafx.event EventHandler]
           [javafx.geometry Point2D]
           [javafx.scene Node Parent Scene SnapshotParameters SubScene]
           [javafx.scene.image PixelFormat WritableImage]
           [javafx.scene.input KeyCode KeyEvent MouseButton MouseEvent PickResult ScrollEvent]
           [javafx.scene.paint Color]
           [javafx.stage Window]
           [javax.imageio ImageIO]))

(set! *warn-on-reflection* true)

;; region util

(defmacro ^:private on-ui-thread [& body]
  `(if (Platform/isFxApplicationThread)
     (do ~@body)
     (let [result# (promise)]
       (Platform/runLater (bound-fn [] (deliver result# (try [::ok (do ~@body)] (catch Throwable t# [::err t#])))))
       (let [[status# value#] @result#]
         (case status# ::ok value# ::err (throw value#))))))

(defn- finite-number? [x]
  (and (number? x) (Double/isFinite (double x))))

;; endregion

;; region props

(defn- bean-stem->prop-key [stem]
  (->> (re-seq #"[A-Z]+(?=$|[A-Z][a-z0-9])|[A-Z]?[a-z0-9]+" stem)
       (map #(.toLowerCase ^String % Locale/ROOT))
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
  "Return a map of supported property values for el (a JavaFX element).

  With no :only, discover all supported keys. :only selects keys to read;
  unsupported keys are omitted, while supported keys with nil values remain.
  Keys are kebab-case names from JavaFX property accessors and observable-list
  getters, e.g. :text, :id, :style-class. Values can include live JavaFX objects.

    (p/props node)
    (p/props node :only [:id :text :visible])"
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

(defn- el? [x]
  (or (identical? ROOT x)
      (instance? Window x)
      (instance? Scene x)
      (instance? Node x)))

(defn ^{:arglists '([el? & {:keys [depth props]}])} tree
  "Return a nested map describing el and its children.

  el may be a Node, Scene, Window, or synthetic root. Omit it to inspect all
  open windows under that root. Each entry has :el (the live object) and a
  vector of :children. :props adds a property map; see props for supported keys.
  :depth limits child traversal: 0 omits :children, 1 includes immediate
  children, and omission traverses the full subtree.

  Traversal follows windows to scenes to scene roots, then node children;
  SubScenes contribute their roots. Start with a small depth to limit output.

    (p/tree :depth 2 :props [:title :id])
    (p/tree window :depth 3 :props [:id :text])
    ;; entry shape: {:el node :props {:id \"name\"} :children [...]}"
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

(defn- execute-query [args]
  (let [candidate (first args)
        arg-is-el (el? candidate)
        el (if arg-is-el candidate ROOT)
        selectors (if arg-is-el (next args) args)]
    (reduce
      (fn execute-step [els {:keys [direct match]}]
        (into []
              (comp
                (mapcat (fn step-children-or-descendants [el]
                          (if direct (-children el) (query-descendants el))))
                (filter match)
                (distinct))
              els))
      [el]
      (normalize-query-steps selectors))))

(defn ^{:arglists '([el? & selectors])} all
  "Return a vector of distinct live elements matching the selectors.

  Optional el is a Node, Scene, Window, or synthetic root; omission searches
  all open windows. Each selector searches descendants of the previous matches,
  excluding the starting elements. Insert > before a selector to search only
  direct children. With no selectors, returns [el] (the root if omitted).

  Selectors:
  - Java class: match instances, e.g. javafx.scene.control.Button.
  - String: #id, .style-class, or combinations such as #id.primary.large.
  - Function or var: predicate on the element. * matches any element.
  - Map: all entries must match. Ordinary keys read props; function/var values
    are predicates on a property, other values use equality (including sets).
    Unsupported properties do not match, even when the expected value is nil.
    :fx.plorer/class matches a class, :fx.plorer/pred tests the whole element,
    and :fx.plorer/style-classes requires all listed CSS classes.

  Use > and * as unquoted clojure.core functions, not keywords or strings.

    (p/all javafx.stage.Window)
    (p/all window \"#form\" > \".field\")
    (p/all window {:text \"Save\"})
    (p/all {:fx.plorer/class javafx.stage.Stage :title \"My app\"})
    (p/all window {:id some? :fx.plorer/style-classes #{\"primary\"}})

  See one to require exactly one match, props to discover property keys."
  [& args]
  (on-ui-thread (execute-query args)))

(defn ^{:arglists '([el? & selectors])} one
  "Return the single matching live element; throw on zero or multiple matches.

  Optional el is a Node, Scene, Window, or synthetic root; omission searches
  all open windows. Selectors search descendants. Accepts the same selectors
  as all, including classes, #id/.class strings, property maps, and predicates.
  See all for chaining and direct-child syntax. Throws IllegalStateException
  unless exactly one element matches; use all to inspect ambiguous matches.

    (p/one javafx.stage.Window)
    (p/one window \"#name\")"
  [& args]
  (let [results (apply all args)]
    (if (= 1 (count results))
      (first results)
      (throw (IllegalStateException. (str "Expected exactly one match, got " (count results)))))))

;; endregion

;; region geometry

(defn point
  "Return scene coordinates [x y] for a relative position within a node.

  x and y must be numbers from 0 to 1 along the node's local layout bounds:
  0,0 is top-left; 0.5,0.5 is center; 1,1 is bottom-right. The Node must belong
  to a Scene. Converts through ancestor transforms and enclosing SubScenes
  into the outer scene's coordinates, suitable for mouse input.

  Reads current geometry on the JavaFX thread. The result is a snapshot;
  layout changes, clipping, or overlapping nodes can affect what gets clicked.
  Exact edges may not be pickable; use e.g. 0.95 to aim just inside an edge.

    (p/point node 0.5 0.5)
    (p/mouse-click! window (p/point node 0.25 0.5) :primary)"
  [node x y]
  (on-ui-thread
    (when-not (instance? Node node)
      (throw (IllegalArgumentException. "Point requires a Node")))
    (when-not (and (finite-number? x) (<= 0 x 1)
                   (finite-number? y) (<= 0 y 1))
      (throw (IllegalArgumentException. "Point coordinates must be numbers from 0 to 1")))
    (when-not (.getScene ^Node node)
      (throw (IllegalStateException. "Point requires a node with a scene")))
    (let [bounds (.getLayoutBounds ^Node node)
          point (.localToScene ^Node node
                               (+ (.getMinX bounds) (* (double x) (.getWidth bounds)))
                               (+ (.getMinY bounds) (* (double y) (.getHeight bounds)))
                               true)]
      (when-not point
        (throw (IllegalStateException. "Point could not be converted to scene coordinates")))
      [(.getX point) (.getY point)])))

;; endregion

;; region key input

(defn- input-scene ^Scene [el]
  (cond
    (identical? ROOT el) (let [windows (vec (Window/getWindows))]
                           (when-not (= 1 (count windows))
                             (throw (IllegalStateException. (str "ROOT input target requires exactly one open window, got " (count windows)))))
                           (recur (first windows)))
    (instance? Scene el) el
    (instance? Window el) (or (.getScene ^Window el)
                              (throw (IllegalStateException. "Input target requires a window with a scene")))
    :else (throw (IllegalArgumentException. "Input target must be ROOT, a Window, or a Scene"))))

(defn- enum-keyword->name [k]
  (let [^String enum-name (str/replace (name k) "-" "_")]
    (.toUpperCase enum-name Locale/ROOT)))

(def ^:private modifier-key-codes #{KeyCode/ALT KeyCode/CONTROL KeyCode/META KeyCode/SHIFT})

(def ^:private ^WeakHashMap scene->input-state (WeakHashMap.))

(def ^:private us-key-texts
  (into {KeyCode/DIGIT0 ["0" ")"]
         KeyCode/DIGIT1 ["1" "!"]
         KeyCode/DIGIT2 ["2" "@"]
         KeyCode/DIGIT3 ["3" "#"]
         KeyCode/DIGIT4 ["4" "$"]
         KeyCode/DIGIT5 ["5" "%"]
         KeyCode/DIGIT6 ["6" "^"]
         KeyCode/DIGIT7 ["7" "&"]
         KeyCode/DIGIT8 ["8" "*"]
         KeyCode/DIGIT9 ["9" "("]
         KeyCode/BACK_QUOTE ["`" "~"]
         KeyCode/MINUS ["-" "_"]
         KeyCode/EQUALS ["=" "+"]
         KeyCode/OPEN_BRACKET ["[" "{"]
         KeyCode/CLOSE_BRACKET ["]" "}"]
         KeyCode/BACK_SLASH ["\\" "|"]
         KeyCode/SEMICOLON [";" ":"]
         KeyCode/QUOTE ["'" "\""]
         KeyCode/COMMA ["," "<"]
         KeyCode/PERIOD ["." ">"]
         KeyCode/SLASH ["/" "?"]
         KeyCode/SPACE [" " " "]
         KeyCode/MULTIPLY ["*" "*"]
         KeyCode/ADD ["+" "+"]
         KeyCode/SUBTRACT ["-" "-"]
         KeyCode/DECIMAL ["." "."]
         KeyCode/DIVIDE ["/" "/"]}
        (concat
          (map (fn [lower upper]
                 [(KeyCode/valueOf (str upper)) [(str lower) (str upper)]])
               "abcdefghijklmnopqrstuvwxyz"
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
          (map (fn [digit]
                 (let [text (str digit)]
                   [(KeyCode/valueOf (str "NUMPAD" digit)) [text text]]))
               "0123456789"))))

(defn- normalize-key-code [key]
  (cond
    (instance? KeyCode key) key
    (keyword? key) (KeyCode/valueOf (enum-keyword->name key))
    :else (throw (IllegalArgumentException. (str "Unsupported key: " key)))))

(defn- input-state [^Scene scene]
  (or (.get scene->input-state scene)
      {:held-key-texts {} :caps-lock false :held-mouse-buttons []}))

(defn- held-modifier-codes [^Scene scene]
  (let [held-key-texts (:held-key-texts (input-state scene))]
    (into #{} (filter #(contains? held-key-texts %)) modifier-key-codes)))

(defn- us-key-text [^KeyCode code shift caps-lock]
  (if-let [texts (get us-key-texts code)]
    (texts (if (if (.isLetterKey code) (not= shift caps-lock) shift) 1 0))
    ""))

(defn- key-event [event-type text ^KeyCode code held-modifier-codes]
  (KeyEvent.
    #_event-type event-type
    #_character (if (= KeyEvent/KEY_TYPED event-type) text KeyEvent/CHAR_UNDEFINED)
    #_text (if (= KeyEvent/KEY_TYPED event-type) "" text)
    #_code (if (= KeyEvent/KEY_TYPED event-type) KeyCode/UNDEFINED code)
    #_shift-down (contains? held-modifier-codes KeyCode/SHIFT)
    #_control-down (contains? held-modifier-codes KeyCode/CONTROL)
    #_alt-down (contains? held-modifier-codes KeyCode/ALT)
    #_meta-down (contains? held-modifier-codes KeyCode/META)))

(defn- dispatch-key! [el event-type key]
  (on-ui-thread
    (let [scene (input-scene el)
          focus-owner (.getFocusOwner ^Scene scene)
          code (normalize-key-code key)]
      (when (nil? focus-owner)
        (throw (IllegalStateException. "Key input requires a focused node")))
      (let [{:keys [held-key-texts caps-lock] :as state} (input-state scene)
            press (= KeyEvent/KEY_PRESSED event-type)
            caps-lock (if (and press (= KeyCode/CAPS code) (not (contains? held-key-texts code)))
                        (not caps-lock)
                        caps-lock)
            text (if press
                   (us-key-text code (contains? held-key-texts KeyCode/SHIFT) caps-lock)
                   (get held-key-texts code ""))]
        (.put scene->input-state scene
              (assoc state
                     :held-key-texts (if press (assoc held-key-texts code text) (dissoc held-key-texts code))
                     :caps-lock caps-lock))
        (let [modifiers (held-modifier-codes scene)]
          (SceneHelper/processKeyEvent scene (key-event event-type text code modifiers))
          (when (and press (not (empty? text))
                     (not-any? modifiers [KeyCode/CONTROL KeyCode/ALT KeyCode/META]))
            (SceneHelper/processKeyEvent scene (key-event KeyEvent/KEY_TYPED text code modifiers)))))
      focus-owner)))

(defn key-press!
  "Press a virtual US keyboard key; return the focus owner at the press.

  el is a Window, Scene, or synthetic root; omission requires one open window.
  Input follows the scene's current focus owner; throws if none exists.
  key is a javafx.scene.input.KeyCode or its
  kebab-case keyword, e.g. :a, :digit1, :enter, :shift, :control, :meta.
  Pair with key-release!, or use key-tap!/key-chord! for complete sequences.

  Uses a US layout regardless of the host layout. Printable keys type text
  unless Control, Alt, or Meta is held. Shift affects case and punctuation;
  :caps toggles Caps Lock for letters, with Shift reversing it. Numpad digits
  always type digits. Num Lock, Alt/Option character
  mappings, dead keys, and IME are not supported.

  Keys stay held until released. Held modifiers apply to later keyboard and
  mouse input in the same scene. Holding a key does not repeat automatically; call
  again to repeat. Caps Lock toggles only once until released and pressed again.

    (p/key-press! window :shift)
    (p/mouse-click! window [50 50] :primary)
    (p/key-release! window :shift)"
  ([key]
   (dispatch-key! ROOT KeyEvent/KEY_PRESSED key))
  ([el key]
   (dispatch-key! el KeyEvent/KEY_PRESSED key)))

(defn key-release!
  "Release a virtual key; return the focus owner at the release.

  el is a Window, Scene, or synthetic root; omission requires one open window.
  key is a javafx.scene.input.KeyCode or kebab-case keyword such as :shift.
  Release follows the scene's current focus owner, which may differ from the
  press target. Throws if there is no focus owner; desktop focus is not required.

  Releasing a modifier stops applying it to later input. Other keys remain held.
  See key-press! for virtual keyboard rules; use key-tap! for a complete tap.

    (p/key-release! window :shift)"
  ([key]
   (dispatch-key! ROOT KeyEvent/KEY_RELEASED key))
  ([el key]
   (dispatch-key! el KeyEvent/KEY_RELEASED key)))

(defn key-tap!
  "Press and release one key; return the focus owner at the release.

  el is a Window, Scene, or synthetic root; omission requires one open window.
  key is a javafx.scene.input.KeyCode or kebab-case keyword, e.g. :a, :enter,
  :tab, :escape, :digit1, :space. Uses a virtual US layout: printable keys type
  text, Shift/Caps Lock affect case, and Control/Alt/Meta suppress typed text.
  See key-press! for full keyboard rules and key-chord! for combinations.

  Input follows the scene's current focus owner, which can change after Tab.
  Throws if no focus owner exists; desktop focus is not required.

    (p/key-tap! :enter)
    (p/key-tap! window :a)"
  ([key]
   (key-tap! ROOT key))
  ([el key]
   (on-ui-thread
     (let [scene (input-scene el)]
       (key-press! scene key)
       (key-release! scene key)))))

(defn key-chord!
  "Press keys in order and release them in reverse order; return nil.

  el is a Window, Scene, or synthetic root; omission requires one open window.
  keys is an ordered collection of javafx.scene.input.KeyCodes or kebab-case
  keywords. Put modifiers first. Uses the virtual US keyboard; see key-press!
  for its rules. Use :meta for Command shortcuts on macOS, :control for Ctrl.

  Input follows the scene's current focus owner. Throws if no focus owner
  exists; desktop focus is not required. Keys in the chord end released;
  other keys remain held.

    (p/key-chord! window [:shift :digit1]) ; type !
    (p/key-chord! window [:control :shift :z])
    (p/key-chord! window [:meta :a])      ; Command+A"
  ([keys]
   (key-chord! ROOT keys))
  ([el keys]
   (on-ui-thread
     (let [scene (input-scene el)]
       (doseq [key keys]
         (key-press! scene key))
       (doseq [key (reverse keys)]
         (key-release! scene key))))))

;; endregion

;; region mouse input

(defn- mouse-event [event-type ^Point2D scene-point ^Point2D screen-point ^MouseButton button held-modifier-codes held-buttons]
  (MouseEvent.
    #_source nil
    #_target nil
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
    #_primary-button-down (contains? held-buttons MouseButton/PRIMARY)
    #_middle-button-down (contains? held-buttons MouseButton/MIDDLE)
    #_secondary-button-down (contains? held-buttons MouseButton/SECONDARY)
    #_back-button-down (contains? held-buttons MouseButton/BACK)
    #_forward-button-down (contains? held-buttons MouseButton/FORWARD)
    #_synthesized false
    #_popup-trigger false
    #_still-since-press false
    #_pick-result (PickResult. nil (.getX scene-point) (.getY scene-point))))

(defn- input-position [^Scene scene position]
  (let [window (.getWindow scene)]
    (when-not (and window (.isShowing window))
      (throw (IllegalStateException. "Mouse input requires a scene in a showing window")))
    (when-not (and (vector? position) (= 2 (count position))
                   (every? finite-number? position))
      (throw (IllegalArgumentException. "Mouse position must be a vector [x y] of finite numbers")))
    (let [[x y] position]
      [(Point2D. (double x) (double y))
       (Point2D. (+ (.getX window) (.getX scene) (double x))
                 (+ (.getY window) (.getY scene) (double y)))])))

(defn- dispatch-mouse! [el position event-type button]
  (on-ui-thread
    (let [scene (input-scene el)
          [scene-point screen-point] (input-position scene position)
          {:keys [held-mouse-buttons] :as state} (input-state scene)
          move (= MouseEvent/MOUSE_MOVED event-type)
          button (cond
                   move (or (peek held-mouse-buttons) MouseButton/NONE)
                   (instance? MouseButton button) button
                   (keyword? button) (MouseButton/valueOf (enum-keyword->name button))
                   :else (throw (IllegalArgumentException. (str "Unsupported mouse button: " button))))
          _ (when (and (not move) (= MouseButton/NONE button))
              (throw (IllegalArgumentException. "Cannot press or release MouseButton/NONE")))
          held-mouse-buttons (cond
                               move held-mouse-buttons
                               (= MouseEvent/MOUSE_PRESSED event-type)
                               (conj (filterv #(not= button %) held-mouse-buttons) button)
                               :else (filterv #(not= button %) held-mouse-buttons))
          event-type (if (and move (seq held-mouse-buttons)) MouseEvent/MOUSE_DRAGGED event-type)
          event (mouse-event event-type scene-point screen-point button (held-modifier-codes scene) (set held-mouse-buttons))
          captured-target (atom nil)
          handler (reify EventHandler
                    (handle [_ event]
                      (swap! captured-target #(or % (.getTarget event)))))]
      (.put scene->input-state scene (assoc state :held-mouse-buttons held-mouse-buttons))
      (.addEventFilter scene event-type handler)
      (try
        (SceneHelper/processMouseEvent scene event)
        @captured-target
        (finally
          (.removeEventFilter scene event-type handler))))))

(defn mouse-move!
  "Move the virtual mouse to position [x y]; return the target or nil.

  el is a Window, Scene, or synthetic root; omission requires one open window.
  The scene must be in a showing window; desktop focus is not required.
  position is a vector of two finite numbers in scene logical pixels from the
  content area's top-left. Use point to obtain coordinates from a node.

  Holding a mouse button drags the original press target. Held keyboard
  modifiers apply. Input in one scene does not affect another.

  Movement is instantaneous and does not move the desktop pointer.
  Call repeatedly for intermediate positions. Outside the scene, the target
  can be nil; dragging continues to target the original press target.

    (p/mouse-press! window [50 50] :primary)
    (p/mouse-move! window [150 100])
    (p/mouse-release! window [150 100] :primary)"
  ([position]
   (mouse-move! ROOT position))
  ([el position]
   (dispatch-mouse! el position MouseEvent/MOUSE_MOVED nil)))

(defn mouse-press!
  "Press a mouse button at position [x y]; return the event target or nil.

  el is a Window, Scene, or synthetic root; omission requires one open window.
  The scene must be in a showing window; desktop focus is not required.
  position is a vector of two finite numbers in scene logical pixels, measured
  from the content area's top-left (excluding window decorations). button is
  :primary, :middle, :secondary, or a javafx.scene.input.MouseButton.

  Presses at the given position, with held keyboard modifiers applied.
  Buttons stay held across calls, including mouse-move!, until released.
  Pair with mouse-release!, or use mouse-click! for a complete click.
  point converts a relative position within a node to scene coordinates.

    (p/mouse-press! window (p/point node 0.5 0.5) :primary)"
  ([position button]
   (mouse-press! ROOT position button))
  ([el position button]
   (dispatch-mouse! el position MouseEvent/MOUSE_PRESSED button)))

(defn mouse-release!
  "Release a mouse button at position [x y]; return the event target or nil.

  el is a Window, Scene, or synthetic root; omission requires one open window.
  The scene must be in a showing window; desktop focus is not required.
  position is a vector of two finite numbers in scene logical pixels from the
  content area's top-left. button is :primary, :middle, :secondary, or a
  javafx.scene.input.MouseButton. Use point to obtain coordinates from a node.

  Release goes to the original press target, even at a different position.
  Other buttons remain held.
  Use mouse-click! for a press/release pair at one position.

    (p/mouse-release! window [50 50] :primary)"
  ([position button]
   (mouse-release! ROOT position button))
  ([el position button]
   (dispatch-mouse! el position MouseEvent/MOUSE_RELEASED button)))

(defn mouse-click!
  "Press and release a mouse button at one position; return the release target or nil.

  el is a Window, Scene, or synthetic root; omission requires one open window.
  The scene must be in a showing window; desktop focus is not required.
  position is a vector [x y] in scene logical pixels from the content area's
  top-left, excluding window decorations. Use point to locate a node.
  button is :primary, :middle, :secondary, or a javafx.scene.input.MouseButton.

  Clicks at the given position, with held keyboard modifiers applied.

    (p/mouse-click! [50 50] :primary)
    (p/mouse-click! window (p/point node 0.5 0.5) :primary)"
  ([position button]
   (mouse-click! ROOT position button))
  ([el position button]
   (on-ui-thread
     (let [scene (input-scene el)]
       (mouse-press! scene position button)
       (mouse-release! scene position button)))))

;; endregion

;; region scroll input

;; SceneHelper has no scroll entry point. The scene's peer listener provides
;; normal picking (including SubScenes) and dispatch without moving the mouse.
;; Scrolling uses state on the enclosing Scene, so a fresh listener can be used
;; for each call; listener fields added in newer releases serve other operations.
(def ^:private scene-peer-listener-constructor
  (delay
    (doto (.getDeclaredConstructor (Class/forName "javafx.scene.Scene$ScenePeerListener")
                                   (into-array Class [Scene]))
      (.setAccessible true))))

(defn scroll!
  "Scroll at position [x y] by dx and dy logical pixels; return the target or nil.

  el is a Window, Scene, or synthetic root; omission requires one open window.
  The scene must be in a showing window; desktop focus is not required.
  position is a vector of two finite numbers in scene logical pixels from the
  content area's top-left. Use point to obtain coordinates from a node.

  dx and dy are finite numbers; fractions are allowed. Negative dx scrolls
  right, negative dy scrolls down; positive values reverse those directions.
  Held keyboard modifiers apply. The control determines the actual movement
  and stops at its scroll limits. Scrolling is immediate, without momentum,
  and does not move the desktop pointer.

    (p/scroll! window (p/point list-view 0.5 0.5) 0 -100)
    (p/scroll! [150 150] -100 0)"
  ([position dx dy]
   (scroll! ROOT position dx dy))
  ([el position dx dy]
   (on-ui-thread
     (let [scene (input-scene el)
           [^Point2D scene-point ^Point2D screen-point] (input-position scene position)
           _ (when-not (and (finite-number? dx) (finite-number? dy))
               (throw (IllegalArgumentException. "Scroll deltas must be finite numbers")))
           modifiers (held-modifier-codes scene)
           ^Constructor constructor @scene-peer-listener-constructor
           ^TKSceneListener listener (.newInstance constructor (object-array [scene]))
           captured-target (atom nil)
           handler (reify EventHandler
                     (handle [_ event]
                       (swap! captured-target #(or % (.getTarget event)))))]
       (.addEventFilter scene ScrollEvent/SCROLL handler)
       (try
         (.scrollEvent listener
                       ScrollEvent/SCROLL
                       #_scroll-x (double dx) #_scroll-y (double dy)
                       #_total-x 0.0 #_total-y 0.0
                       #_multiplier-x 1.0 #_multiplier-y 1.0
                       #_touch-count 0
                       #_text-x 0 #_text-y 0 #_default-text-x 0 #_default-text-y 0
                       (.getX scene-point) (.getY scene-point)
                       (.getX screen-point) (.getY screen-point)
                       #_shift (contains? modifiers KeyCode/SHIFT)
                       #_control (contains? modifiers KeyCode/CONTROL)
                       #_alt (contains? modifiers KeyCode/ALT)
                       #_meta (contains? modifiers KeyCode/META)
                       #_direct false #_inertia false)
         @captured-target
         (finally
           (.removeEventFilter scene ScrollEvent/SCROLL handler)))))))

;; endregion

;; region screenshot

(defn screenshot!
  "Save a screenshot to a temporary PNG; return its absolute path as a string.

  el is a Node, Scene, Window, or synthetic root. Omitting it requires exactly
  one open window. A window captures its scene content, excluding decorations
  and separate popup windows. A node captures itself and its children, without
  surrounding content or overlapping siblings, on a transparent background.

    (p/screenshot!)
    (p/screenshot! window)
    (p/screenshot! (p/one window \"#chart\"))"
  ([]
   (screenshot! ROOT))
  ([el]
   (let [buffered
         (on-ui-thread
           (let [^WritableImage image
                 (if (instance? Node el)
                   (.snapshot ^Node el (doto (SnapshotParameters.) (.setFill Color/TRANSPARENT)) nil)
                   (.snapshot (input-scene el) nil))
                 width (int (.getWidth image))
                 height (int (.getHeight image))
                 pixels (int-array (* width height))
                 buffered (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)]
             (.getPixels (.getPixelReader image) 0 0 width height
                         (PixelFormat/getIntArgbInstance) pixels 0 width)
             (.setRGB buffered 0 0 width height pixels 0 width)
             buffered))
         file (File/createTempFile "plorer-" ".png")]
     (try
       (when-not (ImageIO/write ^BufferedImage buffered "png" file)
         (throw (IOException. "No PNG image writer available")))
       (.getAbsolutePath file)
       (catch Throwable t
         (.delete file)
         (throw t))))))

;; endregion
