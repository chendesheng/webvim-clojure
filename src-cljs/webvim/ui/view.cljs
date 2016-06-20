(ns webvim.ui.view
  (:require [webvim.ui.client :refer [client update-client on-client-change on-buffer-change]]
            [webvim.ui.lib.dom :refer [$new-buffer]]))

(on-client-change
  "active-buf" :switch-buffer-handler
  (fn [_ {bufid :active-buf buffers :buffers} _]
    ($new-buffer bufid)
    (let [buf (buffers bufid)]
      (println (buf :str)))))

