(ns webvim.ui.lib.socket
  (:require [webvim.ui.lib.util :refer [current-time]]))

;https://github.com/mmcgrana/cljs-demo/blob/master/src/cljs-demo/util.cljs
(defn- json-parse
  "Returns ClojureScript data for the given JSON string."
  [line]
  (js->clj (.parse js/JSON line)))

(comment defn connect [url f]
         (letfn [(idelseconds [t] (-> (current-time) (- t) (/ 1000)))
                 (_connect []
                   (doto (js/WebSocket. url)
                     (-> .-onopen (set! (fn [])))
                     (-> .-onclose (set! onclose))
                     (-> .-onmessage
                         (set! (fn [message]
                                 (onsuccess (json-parse (.-data message))))))))
                 (flush-stream
                   [{stream :stream
                     conn :conn :as state}]
                   (if (-> stream empty? not)
                     (let [s (.-readyState conn)]
                       (cond
                         (= s 1)
                         (do (.send conn stream)
                             (assoc state :stream ""))
                         (not= s 0)
                         (assoc state :conn (_connect))
                         :else
                         state))))]
           (fn [{conn :conn stream :stream :as s}]
             (-> s
               (assoc :last-sent (current-time)
                      :stream (str stream key))
               flush-stream))))


(defn- reset-conn [state]
  (doto (@state :conn)
    (-> .-onmessage (set! nil))
    (-> .-onopen (set! nil)))
  (swap! state dissoc :conn))

(declare ^:private flush-stream)
(defn- connect [state]
  (if (-> @state :conn nil? not)
    (reset-conn state))
  (let [conn (js/WebSocket. (@state :url))]
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
      (connect state))))

(defn new-conn [url f]
  (let [state (atom {:url url
                     :stream ""
                     :conn nil
                     :onreceive f})]
    (connect state)
    state))

(defn send [state key]
  (swap! state
         (fn [s]
           (update s :stream #(str % key))))
  (flush-stream state))

