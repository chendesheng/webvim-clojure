(ns webvim.ui.controller.page
  (:require [webvim.ui.lib.event :refer [dispatch-event]]
            [webvim.ui.lib.dom :refer [client-size]]))

(defn handle-size-change [])

(set! js/window.onload
      #(dispatch-event :onload nil))

(set! js/window.onbeforeunload
      #(dispatch-event :unload nil))

(set! js/window.onresize
      #(dispatch-event :onresize (client-size)))
