(ns webvim.lang.html
  (:require [webvim.autoformat :refer [wrap-async-auto-format js-beautify-formatter]]
            [webvim.core.event :refer [listen]])
  (:use webvim.core.lang))

(println "load html language")

(defn- init [buf]
  (-> buf
      (assoc-in [:language :id] ::xml)
      (assoc-in [:language :name] "XML")
      (assoc :tabsize 4)
      (assoc :expandtab false)))

(defn- init-html [buf]
  (-> buf
      (assoc-in [:language :id] ::html)
      (assoc-in [:language :name] "XML")
      (assoc :tabsize 4)
      (assoc :expandtab false)))

(defmethod init-file-type ".html" [buf] (init-html buf))
(defmethod init-file-type ".htm" [buf] (init-html buf))
(defmethod init-file-type ".xml" [buf] (init buf))
(defmethod init-file-type ".svg" [buf] (init buf))
(defmethod init-file-type ".aspx" [buf] (init buf))

(defn- html? [buf]
  (-> buf :language :id (= ::html)))

(listen :init-ex-commands
        (fn [cmds buf]
          (if (html? buf)
            (wrap-async-auto-format cmds (js-beautify-formatter "html"))
            cmds)))
