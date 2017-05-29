(ns webvim.ui.main
  (:require [webvim.ui.lib.event :refer [add-listener add-listener-once dispatch-event]]
            [webvim.ui.lib.socket :refer [open-conn send]]
            [webvim.ui.lib.xhr :refer [xhr-get]]
            [webvim.ui.lib.util :refer [current-path]]
            [webvim.ui.lib.dom :refer [client-size $hiccup measure-text-size]]
            [webvim.ui.controller.page]
            [webvim.ui.view]
            [webvim.ui.controller.input]
            [webvim.ui.controller.page]
            [goog.net.cookies]
            [webvim.ui.client :refer [client update-client]]))

(enable-console-print!)

(defn- ws-url []
  (str "ws://"
       js/window.location.hostname
       (if js/window.location.port
         (str ":" js/window.location.port))
       "/socket?windowId=" (@client :id))) ;TODO: rename window to client

(defn- send-size [[w h]]
  (xhr-get (str "resize/" (@client :id) "/" w "/"  h)))

(defn- adapt [patch]
  (let [adapt-changes (fn [buf]
                        (if (some? (:str buf))
                          (-> buf
                              (assoc :changes [{:a [0 0] :b [0 0] :to (buf :str)}])
                              (dissoc :str))
                          buf))]
    (if (-> patch :buffers some?)
      (update patch :buffers
              (fn [buffers]
                (into
                  {}
                  (map
                    (fn [[bufid buf]]
                      [(-> bufid name js/parseInt)
                       (adapt-changes buf)])
                    buffers))))
      patch)))

(add-listener
  :onload :onload-handler
  (fn [_]
    (open-conn (str (ws-url) "&init=1"))))

(add-listener
  :net-onopen :install-input-events
  (fn [conn]
    (add-listener
      :input-key :input-key-handler
      (fn [key]
        (send conn (str (:active-buffer @client) "!" key))))))

(add-listener
  :net-onopen :send-resize
  (fn [_]
    (let [[_ ch] (measure-text-size "M")
          [cw _] (measure-text-size "1")
                          ;padding (.-offsetHeight ($id "status-bar"))
          padding 24
          px2row-column (fn [[w h]]
                          (let [h (- h padding)]
                            [(js/Math.floor (/ w cw)) (js/Math.floor (/ h ch))]))]
                           ;(println "cw,ch:" cw ch)
      (add-listener :onresize :onresize-handler
                    (let [timer (atom nil)]
                      (fn [sz]
                        (js/clearTimeout @timer)
                        (reset! timer (js/setTimeout #(send-size (px2row-column sz))) 300))))
      (send-size (px2row-column (client-size))))))

(add-listener
  :net-onmessage :update-client
  (fn [resp]
    (doseq [patch (if (vector? resp)
                    resp [resp])]

      ;(println "receive:" patch)
      (update-client (adapt patch)))))

(add-listener
  :net-onfail :reconnect
  (fn [_]
    (open-conn (ws-url))))

(add-listener
  :unload :save-winid
  (fn []
    (println "save client id:" (@client :id))
    (.set goog.net.cookies "windowId" (@client :id) js/Infinity)))

(defn- mmap [f coll]
  (reduce-kv (fn [coll k v]
               (assoc coll k (f v))) nil coll))

(defn fig-reload []
  (let [[_ ch] (measure-text-size "M")
        [cw _] (measure-text-size "1")
        intdiv (fn [a b] (js/Math.floor (/ a b)))
        px2row-column (fn [[w h]]
                        (let [h (- h 24)]
                          [(intdiv w cw) (intdiv h ch)]))]
    (send-size (px2row-column (client-size))))
  ;refresh all
  (let [patch (update @client :buffers
                      (fn [buffers]
                        (mmap #(dissoc % :changes) buffers)))]
    (update-client patch)))


