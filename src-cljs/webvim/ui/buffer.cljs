(ns webvim.ui.buffer)

(def buffers {})

(defn active-buf-id []
  (-> buffers :active :id))
