(ns webvim.ui.lib.socket
  (:require [webvim.ui.lib.util :refer [current-time]]
            [clojure.walk :refer [keywordize-keys]]))

;https://github.com/mmcgrana/cljs-demo/blob/master/src/cljs-demo/util.cljs
(defn- json-parse
  "Returns ClojureScript data for the given JSON string."
  [line]
  (keywordize-keys (js->clj (.parse js/JSON line))))

(defn- reset-conn [state]
  (doto (@state :conn)
    (-> .-onmessage (set! nil))
    (-> .-onopen (set! nil)))
  (swap! state dissoc :conn))

(defn- append-query [url query]
  (if-not (empty? query)
    (str url 
         (if (pos? (.indexOf url "?"))
           "&" "?")
         query)
    url))

(declare ^:private flush-stream)
(defn- connect [state init?]
  (if (-> @state :conn nil? not)
    (reset-conn state))
  (let [conn (js/WebSocket. (append-query ((@state :urlfn)) (if init? "init=1" "")))]
    (swap! state assoc :conn conn)
    (set! (.-onopen conn) 
          #(flush-stream state))
    (set! (.-onmessage conn)
          (fn [message]
            ((@state :onreceive) (json-parse (.-data message)))))))

(defn- flush-stream [state]
  (let [s (-> @state :conn .-readyState)]
    (cond
      (= s 1)
      (let [{conn :conn stream :stream} @state]
        (do (.send conn stream)
            (swap! state assoc :stream "")))
      (not= s 0)
      (connect state nil))))

(defn new-conn [urlfn f]
  (let [state (atom {:urlfn urlfn
                     :stream ""
                     :conn nil
                     :onreceive f})]
    (connect state :init)
    state))

(defn send [state key]
  (swap! state
         (fn [s]
           (update s :stream #(str % key))))
  (flush-stream state))

