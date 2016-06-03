(ns webvim.core.editor
  (:require [webvim.core.event :refer [fire-event]]
            [me.raynes.fs :as fs]
            [webvim.core.utils :refer [uuid]]))

(defonce editor (atom nil))

(def ^:dynamic *window* nil)

(defn update-cwd [s]
  (reset! (*window* :cwd) (str s)))

(defn- create-window []
  (fire-event {:id (uuid) 
               :cwd (atom (str fs/*cwd*))} :create-window))

(defmacro with-window [window & body]
  `(binding [*window* ~window
             fs/*cwd* (-> ~window :cwd deref fs/file)] ~@body))

(defn get-or-create-window [id]
  (if (-> @editor nil?)
    (reset! editor (create-window)))
  @editor)

(defmacro with-window-id [window-id & body]
  `(let [window# (get-or-create-window ~window-id)]
     (binding [*window* window#
               fs/*cwd* (-> window# :cwd deref fs/file)] ~@body)))
