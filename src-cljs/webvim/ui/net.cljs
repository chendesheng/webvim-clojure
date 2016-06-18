(ns webvim.ui.net
  (:require [webvim.ui.render :refer [render]]
            [webvim.ui.buffer :refer [active-buf-id]]
            [webvim.ui.dom :refer [set-props! set-prop!]]))

(defn GET [url success]
  (let [xhr (js/XMLHttpRequest.)]
    (set! (.-onreadystatechange xhr)
          (fn []
            (if (and (= (.-readyState xhr) 4)
                     (= (.-status xhr) 200)
                     success)
              (success))))
    (.open xhr "GET" url true)
    (.send xhr)))

(defn- current-path []
  (->> js/window .-location .-href
       (re-seq #"(?i)^http(?:s?://[^/]*)(/.*)?")
       last))

;https://github.com/mmcgrana/cljs-demo/blob/master/src/cljs-demo/util.cljs
(defn- json-parse
  "Returns ClojureScript data for the given JSON string."
  [line]
  (js->clj (.parse js/JSON line)))

(defn atom-set-map! [obj key val]
  (swap! obj #(assoc % key val)))

(defn current-time []
  (.getTime (new js/Date)))

(defn connect [path]
  (declare _connect)
  (let [state (atom
                {:winid ""
                 :input-buf ""
                 :last-sent (current-time)
                 :conn nil})]

    (letfn [(idelseconds [t] (-> (current-time) (- t) (/ 1000)))
            (wrap-active-bufid [input-buf] (str (active-buf-id) "!" input-buf))
            (flush-input-buffer
              [{input-buf :input-buf
                conn :conn :as state}]
              (if (-> input-buf empty? not)
                (let [s (.-readyState conn)]
                  (cond
                    (= s 1)
                    (do (.send conn input-buf)
                        (assoc state :input-buf ""))
                    (not= s 0)
                    (assoc state :conn (_connect nil))
                    :else
                    state))))
            (_connect [query]
              (set-props!
                (js/WebSocket.
                  (str (current-path)
                       path
                       (or query (str "?windowId=" (@state :winid)))))
                {:onmessage
                 (fn [message]
                   (let [data (json-parse (.-data message))]
                     (doseq [item data]
                       (let [[win buf] (if (vector? item)
                                         item [nil item])]
                         (if (some? win)
                           (atom-set-map! state :winid (win :id)))
                         (render win buf)))))
                 :onclose (fn []
                            (if (< (idelseconds (@state :last-sent)) 2)
                              (atom-set-map! state :conn (_connect nil))))}))]
      (swap! state (fn [{winid :winid :as s}]
                     (assoc s :conn (_connect (str "?init=true&windowId=" winid))))) 
      (fn [key]
        (swap! state (fn [{conn :conn input-buf :input-buf :as s}]
                       (-> s
                           (assoc :last-sent (current-time)
                                  :input-buf (str input-buf key))
                           flush-input-buffer)))))))

