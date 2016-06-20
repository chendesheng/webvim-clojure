(ns webvim.ui.event
  (:require [webvim.ui.lib.watcher :refer [watch-on watch-off trigger watch-once]]))

(def ^:private listeners (atom nil))

(defn add-listener [event id f]
  (swap! listeners watch-on event id f))

(defn remove-listener [event id]
  (swap! listeners watch-off event id))

(defn add-listener-once [event f]
  (swap! listeners watch-once event f))

(defn dispatch-event [event data]
  (trigger @listeners event data))
