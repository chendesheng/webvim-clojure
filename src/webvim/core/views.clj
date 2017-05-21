(ns webvim.core.views
  (:require [webvim.core.event :refer [fire-event listen]]))

(listen :create-window
        (fn [window]
          (assoc window :views 0)))

