(ns build
  (:require [cemerick.pomegranate.aether :as aether]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.tools.build.api :as b])
  (:import [java.io Console]))

(set! *warn-on-reflection* true)

(def lib 'cljfx/plorer)
(def class-dir "target/classes")

(defn- sh!
  [& args]
  (let [{:keys [exit out err]} (apply sh/sh args)]
    (if (zero? exit)
      (str/trim out)
      (throw (ex-info "Command failed" {:args args :exit exit :out out :err err})))))

(defn clean
  [_]
  (b/delete {:path "target"}))

(defn jar
  [_]
  (let [project-version (str "1." (sh! "git" "rev-list" "--count" "HEAD"))
        jar-file (format "target/%s-%s.jar" (name lib) project-version)
        pom-file (format "%s/META-INF/maven/%s/%s/pom.xml" class-dir (namespace lib) (name lib))]
    (clean nil)
    (b/copy-dir {:src-dirs ["src"]
                 :target-dir class-dir})
    (b/write-pom {:class-dir class-dir
                  :lib lib
                  :version project-version
                  :basis (b/create-basis {:project "deps.edn"})
                  :src-dirs ["src"]
                  :scm {:url "https://github.com/cljfx/plorer"
                        :connection "scm:git:https://github.com/cljfx/plorer.git"
                        :developerConnection "scm:git:ssh://git@github.com/cljfx/plorer.git"
                        :tag (sh! "git" "rev-parse" "HEAD")}
                  :pom-data [[:description "A small library for exploring and driving the live JavaFX scene graph from a Clojure REPL."]
                             [:url "https://github.com/cljfx/plorer"]
                             [:licenses
                              [:license
                               [:name "MIT License"]
                               [:url "https://opensource.org/licenses/MIT"]]]]})
    (b/jar {:class-dir class-dir
            :jar-file jar-file})
    {:jar-file jar-file
     :pom-file pom-file
     :version project-version}))

(defn deploy
  [_]
  (let [{:keys [jar-file pom-file version]} (jar nil)]
    (let [^Console console (System/console)]
      (when-not console
        (throw (IllegalStateException. "Missing console for Clojars credentials")))
      (aether/deploy :coordinates [lib version]
                     :jar-file jar-file
                     :pom-file pom-file
                     :repository {"clojars" {:url "https://clojars.org/repo"
                                             :username (.readLine console "Clojars username: " (object-array 0))
                                             :password (String/valueOf (.readPassword console "Clojars token: " (object-array 0)))}}))
    {:jar-file jar-file
     :pom-file pom-file
     :version version}))
