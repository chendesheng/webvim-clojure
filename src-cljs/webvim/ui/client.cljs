(ns webvim.ui.client
  (:require [webvim.ui.lib.event :refer [dispatch-event]]
            [webvim.ui.lib.util :refer [deep-merge]]
            [goog.net.cookies]))

(defn- get-winid []
  (let [id (.get goog.net.cookies "windowId")]
    (println "get client id:" id)
    (.set goog.net.cookies "windowId" "" js/Infinity)
    id))

(defonce client (atom {:id (get-winid)}))

(defn update-client [patch]
  (let [old-client @client
        new-client (swap! client deep-merge patch)]
    ;(println "update-client:" patch)
    (dispatch-event :client-changed [patch old-client new-client])))


