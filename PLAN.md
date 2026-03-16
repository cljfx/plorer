# cljfx.plorer plan

## Definitions

el - Window or Node or other stuff

## props

```clojure
(props el)
(props el :only [:id :text :visible])
```

`props` is minimal and reflective.

- signature: `(props el & {:keys [only]})`
- returns a plain map of supported logical props to current values
- supported props with `nil` values are included; unsupported props are absent
- property existence check is `(contains? (props el) :foo)`
- `:only` limits reads to selected property keys

Property lookup for `:foo-bar`:
- derive `fooBar`
- try zero-arg instance method `fooBarProperty()` first
- then try zero-arg instance method `getFooBar()`, and only accept the result if it is an `ObservableList`

Property keys:
- plain keys mean properties, e.g. `:id`, `:text`, `:visible`
- real JavaFX property names like `:visible`, not `:visible?`
- selector maps use the same property keys as `props`

This covers both requirements:
- enumerate available props: `(keys (props el))`
- check whether a prop exists: `(contains? (props el) :foo)`

## Tree-like structure

```clojure
(tree el)
(tree el :depth 1)
(tree el :props [:id :text :visible])
```

`tree` works on a single `el` only.

- signature: `(tree el & {:keys [depth props]})`
- every tree node includes `:el`, the live object
- `:children` is always present and comes from the same internal `children` relation used by querying
- `:props` is included only when requested
- omitted `:depth` means full recursion
- `:depth 0` means just this node, `:depth 1` includes immediate children

The internal `children` relation is uniform across querying and trees. Examples:
- `Window` -> `[(-> w .getScene)]` when present
- `Scene` -> `[(-> s .getRoot)]` when present
- `Parent` -> `getChildrenUnmodifiable()`
- plus other explorer-visible UI objects like `Tab`, `Menu`, `MenuItem`, etc., as long as they have props and children

Tree shape:

```clojure
{:el el
 :props {:id "assets" :text "Assets"}
 :children [{:el child-1 :children []}
            {:el child-2 :children []}]}
```

## Query

```clojure
(all Window) ; by class
(all "#id")
(all ".style-class")
(all "#assets" Label) ;; every label inside assets
(all VBox > Label) ;; every label directly in VBox

(first {:fx.plorer/class TextField :id "name"})
(one {:fx.plorer/class Button :text "Save"})
```

Selectors are always sequential varargs, interpreted left to right. `>` and `*` are just Clojure vars with special meaning in queries.
Roots are all windows.

Semantics:
- `all` returns a vector of results
- `first` returns the first result or `nil`
- `one` returns the result, throws on many/none
- plain matcher steps traverse descendants
- `>` switches the next matcher step to non-recursive matching
- at the start, `>` means match only against root windows, so `(all > Window)` does not traverse into scenes/nodes

Examples:
- `(all Window)` traverses the whole tree and returns every `Window`
- `(all > Window)` returns root windows only
- `(all VBox)` traverses the whole tree and returns every `VBox`
- `(all VBox > Label)` returns immediate `Label` children of matched `VBox`es

Selector elements:
- Java class: `javafx.scene.control.Button`
- string shorthand: `"#name"` => `{:id "name"}`, `".primary"` => `{:fx.plorer/style-classes #{"primary"}}`, `"#foo.bar.baz"` => `{:id "foo" :fx.plorer/style-classes #{"bar" "baz"}}`
- map: same-element conjunction, e.g. `{:fx.plorer/class TextField :id "name"}` or `{:id "foo" :fx.plorer/style-classes #{"bar"}}`
- function `*`: wildcard
- predicate: `(fn [el] ...)`, or `:fx.plorer/pred` inside a map

Map semantics:
- `:fx.plorer/class` means Java class predicate
- `:fx.plorer/pred` means predicate on the whole candidate
- `:fx.plorer/style-classes` is a set matcher and checks subset inclusion
- plain keys mean properties; function values are predicates, otherwise equality

No bare text strings; use `{:text "Save"}` explicitly. Separate selector steps mean path traversal, while a single map means same-element constraints.

## Inputs

api on nodes:
click! - what about double clicks?
scroll!
key! - or key combo? should allow modifiers...
type!

maybe:
drag! (from / to)
