(ns webvim.ui.render
  (:require [webvim.ui.buffer :refer [active-buf-id]]))

(defn render [win buf]
  (js/console.log win)
  (js/console.log buf))
