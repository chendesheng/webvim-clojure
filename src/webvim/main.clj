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

(defn- active-buffer[]
  (let [b (registers-get registers "%")]
    (if (nil? b) 
      nil
      (@buffer-list (b :id)))))

(defroutes main-routes
  (GET "/" [request] (homepage request))
  (GET "/buf" [] (response [(render nil (or @(active-buffer)
                                            @(second 
                                               (first @buffer-list))))]))
  (GET "/resize/:w/:h" [w h] 
       (swap! window update-in [:viewport] merge {:w (parse-int w) :h (parse-int h)})))

(def ^:private app
  (-> (compojure.handler/api main-routes)
      (wrap-json-response)
      (wrap-resource "public")))

(defn- start-file[f]
  (let [buf @(new-file f)]
    (registers-put! registers "%" {:str f :id (buf :id)})))

(defn- parse-input[body]
  (let [[id keycode] 
        (-> #"(?s)(\d+)\!(.*)"
            (re-seq body)
            first
            rest)]
    [(Integer. id) keycode]))

;(parse-input "123!!\n")
(defonce ^:private ws-out (atom nil))

(defn- change-buffer![buf keycodes ws]
  (println keycodes)
  (let [[newbuf changes] (reduce 
                           (fn[[buf changes] keycode]
                             (let [newbuf (apply-keycode buf keycode)
                                   newchanges (newbuf :changes)]
                               [(assoc newbuf :changes []) 
                                (concat changes newchanges)])) [buf nil] keycodes)
        diff (render buf (assoc newbuf :changes changes))]
    (println diff)
    (jetty/send! ws (json/generate-string diff))
    newbuf))

(defn- handle-socket[req]
  {:on-connect (fn[ws]
                 (reset! ws-out ws))
   :on-text (fn[ws body]
              (let [[id keycode] (parse-input body)]
                ;It's ok to use send here because this server only serve one client.
                (send-off (@buffer-list id) change-buffer! (input-keys keycode) ws)))})

;start app with init file and webserver configs
(defn start[file options]
  (init-keymap-tree)
  (start-file file)
  (jetty/run-jetty #'app 
                   (assoc options :websockets {"/socket" handle-socket})))

(defn -main[& args]
  (start 
    "/tmp/webvim/welcome.txt" 
    {:port 8080 :join? true}))
