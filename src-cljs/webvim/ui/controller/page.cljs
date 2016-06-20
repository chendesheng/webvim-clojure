(ns webvim.ui.controller.page
  (:require [webvim.ui.event :refer [dispatch-event]]))

(defn handle-size-change [])

(set! js/window.onload
      #(dispatch-event :onload nil))

(set! js/window.onbeforeunload
      #(dispatch-event :unload nil))

