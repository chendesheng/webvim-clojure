(ns webvim.lang.css
  (:require [me.raynes.fs :as fs]
            [webvim.autoformat :refer [wrap-async-auto-format js-beautify-formatter]]
            [webvim.core.utils :refer [windows? trim-last-newline]])
  (:use webvim.core.lang
        webvim.core.diff
        webvim.core.event
        webvim.core.rope
        clojure.pprint
        webvim.indent))

(defmethod init-file-type ".css"
  [buf]
  (-> buf
      (assoc-in [:language :id] ::css)
      (assoc-in [:language :name] "CSS")
      (assoc :tabsize 4)
      (assoc :expandtab true)))

(defmethod indent-pos ::css
  [lang r pos]
  (clang-indent r pos))

(defmethod indent-trigger? ::css
  [lang keycode]
  (= keycode "}"))

(defn- css? [buf]
  (-> buf :language :id (= ::css)))

(listen :init-ex-commands
        (fn [cmds buf]
          (if (css? buf)
            (wrap-async-auto-format cmds (js-beautify-formatter "css"))
            cmds)))
