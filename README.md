# cljfx/plorer

A small library for exploring and driving the live JavaFX scene graph from a Clojure REPL.

It is built for inspection-heavy workflows: enumerate supported properties, inspect tree structure, query nodes/windows/scenes, and send synthetic key and mouse events. All public functions are safe to call off the JavaFX UI thread.

## Installation

```clojure
;; published version is derived from the git commit count at release time
{:deps {cljfx/plorer {:mvn/version "1.<commit-count>"}}}
```

## Functionality

Throughout this API, `el` means a JavaFX scene-graph element: a `Node`, `Scene`, or `Window`.

### Props

`props` reads supported logical properties from an element.

Supported keys come from JavaFX `fooProperty()` accessors and observable-list `getFoo()` getters. Unsupported keys are omitted, while supported keys with `nil` values are still included.

```clojure
(props el)                           ;; read all supported props
(props el :only [:id :text :visible]) ;; limit reads to selected props
;;=> {:id "name", :text "Name", :visible true, ...}
```

### Trees

`tree` builds a nested representation of an element and its children.

`cljfx.plorer` models the live scene graph as a tree rooted at a synthetic `ROOT` element: `ROOT` -> all open Windows -> each `Scene` -> scene roots -> descendant nodes. If `el` is omitted, `tree` starts from that `ROOT`. Child lookup follows the live JavaFX structure: `Window -> Scene`, `Scene -> root`, `Parent -> children`, and `SubScene -> root`. Every returned item contains `:el`, `:props` are included only when requested, and `:depth 0` returns only the current node.

```clojure
(tree el)                    ;; full subtree for el
(tree el :depth 1)           ;; el and immediate children
(tree :props [:id :visible]) ;; start from ROOT and include selected props

{:el el
 :props {:id "root"}
 :children [{:el child-1}
            {:el child-2}]}
```

### Queries

`all` and `one` query that same tree.

Queries search the tree described above. Plain selector steps traverse descendants in that tree, while `>` makes the next step direct-child only. `all` returns a vector, and `one` throws unless there is exactly one match.

Selectors can be Java classes such as `Text`, string shorthands such as `"#id"`, `".primary"`, or `"#id.primary.large"`, maps that constrain a single element, predicate functions or vars, and the `*` wildcard.

Map selectors build on `props`:

- Plain keys read logical properties.
- Plain values use equality.
- Function or var values are treated as predicates.
- `:fx.plorer/class` matches by class.
- `:fx.plorer/pred` matches on the whole element.
- `:fx.plorer/style-classes` requires a subset of CSS classes.

```clojure
(all > Window)                                ;; root windows
(all Text)                                    ;; all Text descendants
(all "#assets")                               ;; match by id
(all ".primary")                              ;; match by CSS class
(all "#assets.primary")                       ;; match id and CSS class
(all "#assets" Text)                          ;; all Texts under #assets
(all VBox > Text)                             ;; direct Text children of each VBox
(all {:text "Save"})                          ;; property equality
(all {:id some?})                             ;; property predicate
(all {:fx.plorer/class Text :id "name"})      ;; class and property together
(all {:fx.plorer/style-classes #{"primary"}}) ;; CSS class subset match
(all {:fx.plorer/pred #(instance? Text %)})   ;; whole-element predicate
(one "#name")                                 ;; require exactly one match
```

### Key Input

`key-press!` and `key-release!` send synthetic JavaFX key events.

Keys may be keywords such as `:enter`, `:tab`, `:a`, `:shift`, `:control`, `:alt`, `:meta`, or another `KeyCode` value. Events are dispatched through the scene's current focus owner, and printable key presses also emit `KEY_TYPED`. Calls fail if there is no focused node or if the focused node is outside the requested target subtree.

```clojure
(key-press! el :enter)
(key-release! el :enter)

;; invoke Ctrl/Cmd+A, then Ctrl/Cmd+C on the focused field
(let [shortcut-key (if mac? :meta :control)]
  (key-press! field shortcut-key)
  (key-press! field :a)
  (key-release! field :a)
  (key-press! field :c)
  (key-release! field :c)
  (key-release! field shortcut-key))
```

### Mouse Input

`mouse-press!` and `mouse-release!` send synthetic JavaFX mouse events.

Buttons may be `:primary`, `:middle`, `:secondary`, or a `MouseButton` value. The event is aimed at the visual center of `el`, but actual dispatch goes through JavaFX picking, and the returned value is the picked node. Calls fail if the target has no scene or showing coordinates, and they throw `ex-info` if picking resolves to a different element; failed presses send a balancing release first.

```clojure
(mouse-press! el :primary)
(mouse-release! el :primary)
```
