(ns webvim.core.editor
  (:require [webvim.core.event :refer [fire-event]]
            [me.raynes.fs :as fs]
            [webvim.core.utils :refer [uuid]]))

(defonce editor (atom {:windows {}}))

(def ^:dynamic *window* nil)

(defn current-working-directory []
  @(*window* :cwd))

(defn update-cwd [s]
  (reset! (*window* :cwd) (str s)))

(defn- create-window[]
  (fire-event {:id (uuid) 
               :cwd (atom (str fs/*cwd*))} :create-window))

(defmacro with-window[window & body]
  `(binding [*window* ~window
             fs/*cwd* (-> ~window :cwd deref fs/file)] ~@body))

(defmacro with-window-id[window-id & body]
  `(let [window# (-> @editor :windows (get ~window-id))]
     (binding [*window* window#
               fs/*cwd* (-> window# :cwd deref fs/file)] ~@body)))

(defn get-or-create-window[id]
  (or (-> @editor :windows (get id))
      (let [window (create-window)]
        (swap! editor assoc-in [:windows (window :id)] window)
        window)))

