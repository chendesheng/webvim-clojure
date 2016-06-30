(ns webvim.ui.main
  (:require [webvim.ui.lib.event :refer [add-listener dispatch-event]]
            [webvim.ui.lib.socket :refer [new-conn send]]
            [webvim.ui.lib.xhr :refer [xhr-get]]
            [webvim.ui.lib.util :refer [current-path]]
            [webvim.ui.controller.page]
            [goog.net.cookies]
            [webvim.ui.client :refer [client update-client]]))

(enable-console-print!)

(defn- ws-url []
  (str "ws://" js/window.location.hostname
       (if js/window.location.port
         (str ":" js/window.location.port)
         "") "/socket?window=" (@client :id)))

(add-listener
  :onload :onload-handler
  (fn [_]
    ;window.location.href.replace(/^http(s?:\/\/[^/]*)(\/.*)?/i, "ws$1") + path + (query || '?windowId=' + _windowId) 
    (let [conn (new-conn ws-url (fn [alldata]
                                  (doseq [data alldata]
                                      ;TODO: get rid of this, make server generate right schema
                                    (println "receive:" data)
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
                                          active-buf (:active-buf @client)
                                          patch (merge (if-not (nil? buf)
                                                         (if (or (nil? bufid)
                                                                 (= active-buf bufid))
                                                           {:buffers {active-buf buf}}
                                                           {:active-buf bufid
                                                            :layouts [:| bufid]
                                                            :buffers {bufid buf}})) win)]
                                      (dispatch-event :server-message patch)
                                      (doseq [[bufid buf] (@client :buffers)]
                                        (swap! client assoc bufid (dissoc buf :changes)))))))]
      (add-listener
        :input-key :input-key-handler
        (fn [key]
          (send conn (str (:active-buf @client) "!" key)))))))

(add-listener
  :unload :save-winid
  (fn []
    (.set goog.net.cookies "windowId" (@client :id) js/Infinity)))

(add-listener
  :onresize :onresize-handler
  #(update-client {:size %}))

(add-listener :server-message :server-message-handler update-client)
