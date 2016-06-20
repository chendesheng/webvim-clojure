(ns webvim.ui.view.render
  (:require [webvim.ui.client :refer [client on-client-change off-client-change update-client on-buffer-change]]))

(on-buffer-change
  :mode
  (fn [buf _ _]
    (case (buf :mode)
      :normal nil
      :insert nil)))

