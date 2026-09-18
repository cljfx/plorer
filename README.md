# cljfx/plorer

[![CI](https://github.com/cljfx/plorer/actions/workflows/ci.yml/badge.svg)](https://github.com/cljfx/plorer/actions/workflows/ci.yml)
[![Clojars Project](https://img.shields.io/clojars/v/io.github.cljfx/plorer.svg)](https://clojars.org/io.github.cljfx/plorer)
[![License](https://img.shields.io/github/license/cljfx/plorer)](LICENSE)

Explore and drive a running JavaFX application from its Clojure REPL. Inspect controls, interact with the application, and take screenshots. Useful for people and coding agents with REPL access.

## Installation

See the [latest release on Clojars](https://clojars.org/io.github.cljfx/plorer) for dependency coordinates.

## Getting started

Run these forms in your application's REPL. Start with `tree` to see what's in a window, then use `one` to find a control and `props` to inspect it. You can click, type, hover, scroll, and capture the result.

This example assumes one open window. Replace `#name` and `#items` with IDs from your application's tree for a text field and a scrollable list.

```clojure
(require '[cljfx.plorer :as p])

;; Find a window and inspect its contents.
(p/all javafx.stage.Window)
(def window (p/one javafx.stage.Window))
(p/tree window :depth 3 :props [:id :text])

;; Find a field and click its center.
(def field (p/one window "#name"))
(p/mouse-click! window (p/point field 0.5 0.5) :primary)

;; Type "hi!".
(doseq [key [:h :i]]
  (p/key-tap! window key))
(p/key-chord! window [:shift :digit1])

;; Inspect the result.
(p/props field :only [:text])

;; Hover over a scrollable list and scroll down.
(def items (p/one window "#items"))
(p/mouse-move! window (p/point items 0.5 0.5))
(p/scroll! window (p/point items 0.5 0.5) 0 -100)

;; Take a screenshot.
(p/screenshot! window)
```
