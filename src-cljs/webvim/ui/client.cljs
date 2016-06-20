(ns webvim.ui.client
  (:require [webvim.ui.lib.model :refer [on-model-change off-model-change apply-patch]]))

(def client (atom nil))
(def ^:private client-watchers (atom nil))

(defn on-client-change [path watcher-id f]
  (swap! client-watchers
         #(on-model-change % path watcher-id f)))

(defn off-client-change [path watcher-id]
  (swap! client-watchers
         #(off-model-change % path watcher-id)))

(defn update-client [diff]
  (swap! client #(apply-patch % diff @client-watchers)))

(defn on-buffer-change [k f]
  (let [prop (name k)]
    (on-client-change
      (str "buffers.*." prop)
      (symbol (str "buffer-" prop "-change-handler"))
      (fn [[_ bufid] client old-client]
        (f (-> client :buffers bufid) client old-client)))))


