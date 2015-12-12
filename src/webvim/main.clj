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
     [:link {:href "monokai.css" :rel "stylesheet"}]]
    [:body]))

(defn- active-buffer[]
  (let [b (registers-get registers "%")]
    (if (nil? b)
      nil
      (@buffer-list (b :id)))))

(defroutes main-routes
  (GET "/" [request] (homepage request))
  (GET "/buf" [] (response (ui-buf)))
  (GET "/resize/:w/:h" [w h]
       (send ui-agent 
             (fn[ui]
               (update-in ui [:viewport] merge {:w (parse-int w) :h (parse-int h)})))))

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
            newbuf (send-buf! (assoc buf :changes changes))]
        (let [nextid (newbuf :nextid)]
          (if (nil? nextid) newbuf
            (do
              (let [anextbuf (@buffer-list nextid)]
                ;(println "nextid" nextid)
                (if-not (nil? anextbuf)
                  (send anextbuf
                            (fn[buf]
                              (send-buf! buf)))))
              (dissoc newbuf :nextid)))))
      (catch Exception e
             (println e)
             (.printStackTrace e)
             (send-buf! (assoc buf :message (str e)))))))

(defn- handle-socket[req]
  {:on-connect (fn[ws]
                 (send ui-agent (fn[ui ws]
                                  (assoc ui :ws ws)) ws))
   :on-text (fn[ws body]
              (let [[id keycode] (parse-input body)]
                (send (@buffer-list id) change-buffer! (input-keys keycode))))})

(defn- write-client![ui diff]
  (let [ws (ui :ws)]
    (if-not (nil? ws)
      (jetty/send! ws (json/generate-string diff)))))

;start app with init file and webserver configs
(defn start[file options]
  (send ui-agent (fn[ui]
                   (assoc ui :render! write-client!)))
  (init-keymap-tree)
  (start-file file)
  (jetty/run-jetty #'app
                   (assoc options :websockets {"/socket" handle-socket})))

(defn -main[& args]
  (start
    "/tmp/webvim/welcome.txt"
    {:port 8080 :join? true}))
