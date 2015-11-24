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

;editor holds all global states
;(def editor 
;  {:buffers (atom{})
;   :root-keymap (atom{})
;   ;global registers. Never access this directly, always use buffer's :registers instead
;   :registers (atom{})
;   ;the current editting buffer, when receving keys from client send keycode to this buffer's :chan-in
;   ;switch to another buffer is very easy, just do (reset! actvive-buffer b)
;   ;TODO Serve more than one client at once, make eash session different active-buffer
;   :active-buffer-id (atom int)
;   ;one server only serve one window at one time
;   :window (atom{:viewport {:w 0 :h 0}})
;   :chan-in (async/chan)
;   :chan-out (async/chan)})

(defn- update-buffer [buf]
  ;not contains? means already deleted
  (swap! buffer-list 
         #(if (contains? % (:id buf))
            (assoc % (:id buf) buf) %)))

(defn- edit [keycode]
  (let [before (active-buffer)]
    (async/>!! (:chan-in before) keycode)
    (let [after (async/<!! (:chan-out before))]
      (update-buffer after)
      ;Always write (active-buffer) back because active-buffer-id may change by current key
      (render before (active-buffer)))))

(defn- handle-keys
  [s]
  (reduce
    (fn [changes keycode]
      (conj changes (edit keycode))) [] (input-keys s)))

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

(defroutes main-routes
  (GET "/" [request] (homepage request))
  (GET "/buf" [] (response [(render nil (active-buffer))]))
  (GET "/resize/:w/:h" [w h] 
       (swap! window update-in [:viewport] merge {:w (parse-int w) :h (parse-int h)})))

(def app
  ;(wrap-json-response main-routes))
  (-> (compojure.handler/api main-routes)
      (wrap-json-response)
      (wrap-resource "public")))

(defn- open-welcome-file[]
  (let [id (reset! active-buffer-id 
                   (-> "/tmp/webvim/welcome.txt"
                       new-file
                       :id))]
    (registers-put!
      registers
      "%" 
      {:str "/tmp/webvim/welcome.txt" :id id})))

(def socket-handler
  {:on-text (fn[ws keycode]
              (println keycode)
              (jetty/send! 
                ws 
                (time 
                  (json/generate-string
                    (handle-keys keycode)))))})

(defn run-webserver[port block?]
  (jetty/run-jetty #'app {:port port 
                          :join? block?
                          :websockets {"/socket" socket-handler}}))

(defn -main[& args]
  (init-keymap-tree)
  (open-welcome-file)
  (run-webserver 8080 true))
