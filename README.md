# cljfx/plorer

[![CI](https://github.com/cljfx/plorer/actions/workflows/ci.yml/badge.svg)](https://github.com/cljfx/plorer/actions/workflows/ci.yml)
[![Clojars Project](https://img.shields.io/clojars/v/io.github.cljfx/plorer.svg)](https://clojars.org/io.github.cljfx/plorer)
[![License](https://img.shields.io/github/license/cljfx/plorer)](LICENSE)

Explore and drive a running JavaFX application from its Clojure REPL. Useful for coding agents with REPL access: inspect the live scene graph, find controls, and interact through synthetic keyboard and mouse events without desktop focus.

## Installation

Using `deps.edn`:

```clojure
io.github.cljfx/plorer {:mvn/version "1.25"}
```

Using Leiningen:

```clojure
[io.github.cljfx/plorer "1.25"]
```

## Getting started

Run this in your application's REPL. The example assumes one open window and a text field whose ID is `name`; adapt the selector to your application.

```clojure
(require '[cljfx.plorer :as p])

;; Find a window and inspect its contents.
(p/all javafx.stage.Window)
(def window (p/one javafx.stage.Window))
(p/tree window :depth 3 :props [:id :text])

;; Find a field and click its center.
(def field (p/one window "#name"))
(p/mouse-click! window (p/point field 0.5 0.5) :primary)

;; Type "hi!" using the virtual US keyboard.
(doseq [key [:h :i]]
  (p/key-tap! window key))
(p/key-chord! window [:shift :digit1])

;; Inspect the result.
(p/props field :only [:text])
```

- Calls run synchronously on the JavaFX thread; no `Platform/runLater` is needed. Mouse input requires a showing window, but desktop focus is not required.
- Input targets a window or scene. Omitting the target requires exactly one open window. Keyboard events follow the scene's current focus owner.
- Mouse positions are `[x y]` in scene logical pixels, measured from the content area's top-left. `point` converts relative positions within a node's layout bounds to those coordinates: `0 0` is top-left, `0.5 0.5` is center, and `1 1` is bottom-right.
