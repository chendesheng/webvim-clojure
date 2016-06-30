(ns webvim.ui.main
  (:require [webvim.ui.lib.event :refer [add-listener add-listener-once dispatch-event]]
            [webvim.ui.lib.socket :refer [open-conn send]]
            [webvim.ui.lib.xhr :refer [xhr-get]]
            [webvim.ui.lib.util :refer [current-path]]
            [webvim.ui.lib.dom :refer [client-size]]
            [webvim.ui.controller.page]
            [goog.net.cookies]
            [webvim.ui.client :refer [client update-client]]))

(enable-console-print!)

(defn- ws-url []
  (str "ws://"
       js/window.location.hostname
       (if js/window.location.port
         (str ":" js/window.location.port))
       "/socket?window=" (@client :id)))

(defn- send-size [[w h]]
  (xhr-get (str "resize/" (@client :id) "/" w "/"  h)))

;;will drop this after server side overhaul
(defn- patch-adapt [data]
  (let [[win buf1] (if (vector? data) data [nil data])
        buf2 (if (-> buf1 :mode zero?)
               (assoc buf1 :line-buffer nil)
               buf1)
        buf (if (some? (buf2 :str))
              (-> buf2
                  (assoc :changes [{:a [0 0] :b [0 0] :to (buf2 :str)}])
                  (dissoc :str))
              buf2)
        _ (println "buf:" buf)
        bufid (:id buf)
        active-buf (:active-buf @client)]
    (merge (if-not (nil? buf)
             (if (or (nil? bufid)
                     (= active-buf bufid))
               {:buffers {active-buf buf}}
               {:active-buf bufid
                :layouts [:| bufid]
                :buffers {bufid buf}})) win)))

(add-listener
  :onload :onload-handler
  (fn [_]
    (add-listener :net-onopen :install-input-events
                  (fn [conn]
                    (add-listener
                      :input-key :input-key-handler
                      (fn [key]
                        (send conn (str (:active-buf @client) "!" key))))))
    (add-listener-once :net-onopen
                       (fn [_]
                         (let [timer (atom nil)]
                           (add-listener :onresize :onresize-handler
                                         (fn [sz]
                                           (js/clearTimeout @timer)
                                           (reset! timer (js/setTimeout #(send-size sz) 300))))
                           (send-size (client-size)))))
    (add-listener :net-onmessage :update-client
                  (fn [resp]
                    (doseq [patch (map patch-adapt resp)]
                      (println "receive:" patch)
                      (update-client patch))))
    (add-listener :net-onfail :reconnect
                  (fn [_]
                    (open-conn (ws-url))))
    (open-conn (str (ws-url) "&init=1"))))

(add-listener
  :unload :save-winid
  (fn []
    (println "save client id:" (@client :id))
    (.set goog.net.cookies "windowId" (@client :id) js/Infinity)))



