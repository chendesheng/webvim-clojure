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
        webvim.core.register
        webvim.core.keys
        webvim.core.utils
        webvim.keymap
        webvim.keymap.action
        webvim.core.ui
        (compojure handler [core :only (GET POST defroutes)])
        (hiccup [page :only (html5)])
        ring.middleware.resource
        ring.util.response
        ring.middleware.json
        ring.middleware.content-type))

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
     [:link {:href "ubuntu-mono.css" :rel "stylesheet"}]
     [:link {:href "main.css" :rel "stylesheet"}]
     [:link {:href "monokai.css" :rel "stylesheet"}]
     [:script {:type "text/json" :id "init-buf"} (json/generate-string (ui-buf))]]
    [:body]))

(defroutes main-routes
  (GET "/" [request] (homepage request))
  (GET "/resize/:w/:h" [w h]
       (send ui-agent 
             (fn[ui w h]
               (update-in ui [:viewport] merge {:w w :h h})) (parse-int w) (parse-int h))))

(def ^:private app
  (-> (compojure.handler/api main-routes)
      (wrap-json-response)
      (wrap-resource "public")
      (wrap-content-type)))

(defn- start-file[f]
  (let [buf @(new-file f)]
    (registers-put! registers "%" {:str f :id (buf :id)})
    (send-buf! buf)))

(defn- parse-input[body]
  (let [[id keycode]
        (-> #"(?s)(\d+)\!(.*)"
            (re-seq body)
            first
            rest)]
    [(Integer. id) keycode]))

;(parse-input "123!!\n")
(defn- change-buffer![buf keycodes]
  (time
    (try
      (let [[buf changes] (apply-keycodes buf (buf :root-keymap) keycodes)
            newbuf (send-buf! (assoc buf :changes changes))
            nextid (newbuf :nextid)]
        (if (nil? nextid) newbuf
          (do
            (let [anextbuf (@buffer-list nextid)]
            ;(println "nextid" nextid)
              (if-not (nil? anextbuf)
                (send anextbuf
                      (fn[buf]
                        (send-buf! buf)))))
            (dissoc newbuf :nextid))))
      (catch Exception e
        (println e)
        (.printStackTrace e)
        (let [sw (java.io.StringWriter.)
              pw (java.io.PrintWriter. sw)]
          (.printStackTrace e pw)
          (-> buf
              (dissoc :nextid)
              (write-output (str sw) true)))))))

(defn- handle-socket[req]
  {:on-connect (fn[ws]
                 (send ui-agent (fn[ui ws]
                                  (assoc ui :ws ws)) ws))
   :on-text (fn[ws body]
              (let [[id keycode] (parse-input body)]
                (send (@buffer-list id) change-buffer! (input-keys keycode))))})

(defn- write-client![ui diff]
  (let [ws (ui :ws)]
    (try
      (jetty/send! ws (-> ui :queue (vconj diff) json/generate-string))
      (dissoc ui :queue)
      (catch Exception e
        (update-in ui [:queue] vconj diff)))))

(defonce ^:private web-server (atom nil))

;start app with init file and webserver configs
(defn start[file options]
  (init-keymap-tree)
  (if-not (empty? file) (start-file file))
  (send ui-agent (fn[ui]
                   (assoc ui :render! write-client!)))
  (println "start web server:" (options :port))
  (reset! web-server
          (jetty/run-jetty #'app
                   (assoc options :websockets {"/socket" handle-socket}))))

(defn stop[]
  (println "stop web server")
  (jetty/stop-server @web-server))

(defn -main[& args]
  (start
    "/tmp/webvim/welcome.txt"
    {:port 8080 :join? true}))
