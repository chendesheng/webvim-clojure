(ns webvim.ui.client
  (:require [webvim.ui.lib.model :refer [on-model-change off-model-change apply-patch]]
            [goog.net.cookies]))

(defn- get-winid []
  (let [id (.get goog.net.cookies "windowId")]
    (.set goog.net.cookies "windowId" "" js/Infinity)
    id))

(def client (atom {:id (get-winid)}))
(def ^:private client-watchers (atom nil))

(defn on-client-change [path watcher-id f]
  (swap! client-watchers
         #(on-model-change % path watcher-id f)))

(defn off-client-change [path watcher-id]
  (swap! client-watchers
         #(off-model-change % path watcher-id)))

(defn update-client [patch]
  (println "update-client:" patch)
  (swap! client #(apply-patch % patch @client-watchers)))

(defn on-buffer-change [k f]
  (let [prop (name k)]
    (on-client-change
      (str "buffers.*." prop)
      (symbol (str "buffer-" prop "-change-handler"))
      (fn [[_ bufid] client old-client]
        (-> client :buffers (get bufid) (f client old-client))))))


