(ns webvim.ui.main
  (:require [webvim.ui.event :refer [add-listener dispatch-event]]
            [webvim.ui.lib.socket :refer [new-conn send]]
            [webvim.ui.lib.xhr :refer [xhr-get]]
            [webvim.ui.lib.util :refer [current-path]]
            [webvim.ui.controller.page]
            [webvim.ui.client :refer [client update-client on-client-change on-buffer-change]]))

(enable-console-print!)

(defn get-winid []
  (.get goog.net.cookies "windowId"))

(defn save-winid [winid]
  (.set goog.net.cookies "windowId" winid js/Infinity))

(defn- ws-url []
  (str "ws://" js/window.location.hostname
       (if js/window.location.port
         (str ":" js/window.location.port)
         "") "/socket?init=1"))

(add-listener
  :onload :onload-handler
  (fn [_]
    ;window.location.href.replace(/^http(s?:\/\/[^/]*)(\/.*)?/i, "ws$1") + path + (query || '?windowId=' + _windowId) 
    (let [conn (new-conn (ws-url) #(dispatch-event :server-message %))]
      (add-listener
        :input-key :input-key-handler
        (fn [key]
          (send conn key))))))

(add-listener
  :onresize :onresize-handler
  #(update-client {:size %}))

(add-listener
  :server-message :server-message-handler
  (fn [buf]
    (println buf)
    (let [bufid (buf :id)
          active-buf (@client :active-buf)]
      (update-client
        (if (or (nil? bufid)
                (= active-buf bufid))
          {:buffers {active-buf buf}}
          {:active-buf bufid
           :buffers {bufid buf}})))))

(on-client-change
  "size" :size-change-handler
  (fn [_ {{width :width height :height} :size} _]
    (xhr-get (str "/size/" width "/" height) nil)))

(on-buffer-change :mode (fn [buf]))
