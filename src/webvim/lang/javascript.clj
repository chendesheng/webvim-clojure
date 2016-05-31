(ns webvim.lang.javascript
  (:require [me.raynes.fs :as fs]
            [clojure.java.shell :refer [sh]]
            [webvim.autoformat :refer [wrap-async-auto-format js-beautify-formatter]]
            [webvim.core.utils :refer [windows? trim-last-newline]])
  (:use webvim.core.lang
        webvim.core.rope
        webvim.core.diff
        webvim.core.event
        clojure.pprint
        webvim.indent))

(println "load javascript language")

(defmethod init-file-type ".js"
  [buf]
  (-> buf
      (assoc-in [:language :id] ::javascript)
      (assoc-in [:language :name] "JavaScript")
      (assoc :tabsize 4)
      (assoc :expandtab true)))

(defmethod indent-pos ::javascript
  [buf]
  (clang-indent buf))

(defmethod indent-trigger? ::javascript
  [lang keycode]
  (= keycode "}"))

(defn- javascript? [buf]
  (-> buf :language :id (= ::javascript)))

(defn- json? [buf]
  (-> buf :language :id (= ::json)))

(comment defn- jsfmt [s]
         (println "jsfmt")
         (clojure.java.shell/sh "jsfmt" "-d" :in s))

(listen :init-ex-commands
        (fn [cmds buf]
          (if (or (javascript? buf)
                  (json? buf))
            (wrap-async-auto-format cmds (js-beautify-formatter "js"))
            cmds)))
