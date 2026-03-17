(ns cljfx.plorer
  (:require [clojure.string :as str])
  (:import [java.beans Introspector]
           [java.lang.reflect Method Modifier]
           [javafx.beans.value ObservableValue]
           [javafx.collections ObservableList]))

;; region props

(set! *warn-on-reflection* true)

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
  (let [prop-getters (class-prop-getters (class el))]
    (reduce
      (fn [m prop-key]
        (if-some [getter (get prop-getters prop-key)]
          (assoc m prop-key (getter el))
          m))
      {}
      (or only (keys prop-getters)))))

;; endregion
