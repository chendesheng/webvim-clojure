(ns webvim.main
  (:require [clojure.core.async :as async]
            [ring.util.response :as response]
            [cheshire.core :as json]
            [ring.adapter.jetty9 :as jetty])
  (:use clojure.pprint
        webvim.lang.default
        webvim.lang.clojure ;TODO: load language setting dynamically
        webvim.lang.javascript
        webvim.lang.css
        webvim.core.buffer
        webvim.core.serve
        webvim.core.register
        webvim.core.keys
        webvim.keymap
        webvim.keymap.action
        webvim.render
        (compojure handler [core :only (GET POST defroutes)])
        (hiccup [page :only (html5)])
        ring.middleware.resource
        ring.util.response
        ring.middleware.json))

;send one keycode
(defn- handle-key [buf keycode]
  (async/>!! (:chan-in buf) keycode)  ;blocking
  (let [newbuf (async/<!! (:chan-out buf))]
    (if (nil? (@buffer-list (buf :id)))
      (let[]
        (async/close! (buf :chan-in))
        [(render nil (@buffer-list (newbuf :nextid)))])
      (let[res (render buf newbuf)]
        (save-buffer! newbuf)
        (if (-> newbuf :nextid nil?)
          [res]
          [res (render nil (@buffer-list (newbuf :nextid)))])))))

;send sequence of keycodes one by one
(defn- handle-keys
  [id s]
  (let [buf (@buffer-list id)]
    (println (buf :filepath))
    (reduce
      (fn [changes keycode]
        (concat changes (handle-key buf keycode))) [] (input-keys s))))

(defn- parse-int [s]
  (Integer. (re-find #"\d+" s)))

(defn- homepage
  [request]
  (html5 
    [:head
     [:script {:src "jquery.js" :type "text/javascript"}]
     [:script {:src "socket.js" :type "text/javascript"}]
     [:script {:src "utils.js" :type "text/javascript"}]
     [:script {:src "dom.js" :type "text/javascript"}]
     [:script {:src "keycode.js" :type "text/javascript"}]
     [:script {:src "keymap.js" :type "text/javascript"}]
     [:script {:src "keyboard.js" :type "text/javascript"}]
     [:script {:src "syntax/clojure.js" :type "text/javascript"}]
     [:script {:src "syntax/css.js" :type "text/javascript"}]
     [:script {:src "syntax/xml.js" :type "text/javascript"}]
     [:script {:src "syntax/javascript.js" :type "text/javascript"}]
     [:script {:src "highlight.js" :type "text/javascript"}]
     [:script {:src "main.js" :type "text/javascript"}]
     [:link {:href "main.css" :rel "stylesheet"}]
     [:link {:href "monokai.css" :rel "stylesheet"}]]
    [:body]))

(defn active-buffer[]
  (let [b (registers-get registers "%")]
    (if (nil? b) 
      nil
      (@buffer-list (b :id)))))

(defroutes main-routes
  (GET "/" [request] (homepage request))
  (GET "/buf" [] (response [(render nil (or (active-buffer)
                                            (second 
                                              (first @buffer-list))))]))
  (GET "/resize/:w/:h" [w h] 
       (swap! window update-in [:viewport] merge {:w (parse-int w) :h (parse-int h)})))

(def app
  (-> (compojure.handler/api main-routes)
      (wrap-json-response)
      (wrap-resource "public")))

(defn- open-welcome-file[]
  (let [f "/tmp/webvim/welcome.txt" 
        id (-> f new-file :id)]
    (registers-put! registers "%" {:str f :id id})))

(defn parse-input[body]
  (let [[id keycode] 
        (-> #"(?s)(\d+)\!(.*)"
            (re-seq body)
            first
            rest)]
    [(Integer. id) keycode]))

;(parse-input "123!!\n")

(defn handle-socket[req]
  {:on-text (fn[ws body]
              (let [[id keycode] (parse-input body)]
                (println keycode)
                (jetty/send! 
                  ws 
                  (time 
                    (json/generate-string
                      (handle-keys id keycode))))))})

(defn run-webserver[port block?]
  (jetty/run-jetty #'app {:port port 
                          :join? block?
                          :websockets {"/socket" handle-socket}}))

(defn -main[& args]
  (init-keymap-tree)
  (open-welcome-file)
  (run-webserver 8080 true))
