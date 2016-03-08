(ns webvim.main
  (:require [clojure.core.async :as async]
            [ring.util.response :as response]
            [cheshire.core :as json]
            [ring.adapter.jetty9 :as jetty])
  (:use clojure.pprint
        webvim.lang.clojure ;TODO: load language setting dynamically
        webvim.lang.javascript
        webvim.lang.css
        webvim.lang.sql
        webvim.lang.go
        webvim.lang.csharp
        webvim.lang.html
        webvim.core.buffer
        webvim.core.rope
        webvim.core.event
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
     [:script {:src "syntax/sql.js" :type "text/javascript"}]
     [:script {:src "syntax/go.js" :type "text/javascript"}]
     [:script {:src "syntax/cs.js" :type "text/javascript"}]
     [:script {:src "syntax/javascript.js" :type "text/javascript"}]
     [:script {:src "highlight.js" :type "text/javascript"}]
     [:script {:src "main.js" :type "text/javascript"}]
     [:script {:src "render/autocompl.js" :type "text/javascript"}]
     [:script {:src "render/cursor.js" :type "text/javascript"}]
     [:script {:src "render/gutter.js" :type "text/javascript"}]
     [:script {:src "render/offscreen/changes.js" :type "text/javascript"}]
     [:script {:src "render/offscreen/lines.js" :type "text/javascript"}]
     [:script {:src "render/offscreen/pos.js" :type "text/javascript"}]
     [:script {:src "render/selection.js" :type "text/javascript"}]
     [:script {:src "render/viewport.js" :type "text/javascript"}]
     [:script {:src "render/watchers.js" :type "text/javascript"}]
     [:link {:href "ubuntu-mono.css" :rel "stylesheet"}]
     [:link {:href "main.css" :rel "stylesheet"}]
     [:link {:href "monokai.css" :rel "stylesheet"}]
     ;TODO: use base64 endocde
     ;[:script {:type "text/json" :id "init-buf"} (json/generate-string (ui-buf))]]
     ]
    [:body]))

(defroutes main-routes
  (GET "/buf" [request] (json/generate-string (ui-buf)))
  (GET "/" [request] (homepage request))
  (GET "/resize/:w/:h" [w h]
    (send ui-agent 
          (fn [ui w h]
            (update-in ui [:viewport] assoc :w w :h h)) (parse-int w) (parse-int h))))

(def ^:private app
  (-> (compojure.handler/api main-routes)
      (wrap-json-response)
      (wrap-resource "public")
      (wrap-content-type)))

(defn- start-file [f]
  (println "start-file:" f)
  (let [buf @(new-file f)]
    (registers-put! "%" {:str f :id (buf :id)})
    (send-buf! buf)))

(defn- parse-input [body]
  (let [[id keycode]
        (-> #"(?s)(\d+)\!(.*)"
            (re-seq body)
            first
            rest)]
    [(Integer. id) keycode]))

(defn- pretty-trace
  "convert error trace to file path with line number"
  [err]
  (let [st (-> err .getStackTrace seq)]
    (map (fn [line]
           (let [class (.getClassName line)
                ;method (.getMethodName line)
                 file (.getFileName line)
                 linenum (.getLineNumber line)
                 appendsrc (fn [name]
                             (if (re-test #"^webvim" name)
                               (str "src/" name)
                               name))]
             (str (-> class
                      (clojure.string/replace #"\$.*" "")
                      ;FIXME: handle class name doesn't contains dot 
                      (clojure.string/replace #"[.][^.]*$" "")
                      (clojure.string/replace "." "/")
                      (clojure.string/replace "_" "-")
                      appendsrc) "/" file ":" linenum))) st)))

(defn- change-buffer! [buf keycodes]
  (time
    (try
      (let [oldbuf buf
            buf (-> buf (apply-keycodes keycodes) send-buf!)
            nextid (buf :nextid)]
        ;TODO: add event here
        (if (or (not= (oldbuf :str) (buf :str)) (not= (oldbuf :pos) (buf :pos)))
          (if (= (buf :mode) insert-mode)
            (buf-match-bracket buf (-> buf :pos dec))
            (buf-match-bracket buf)))
        (if (nil? nextid) buf
            (do
              (let [anextbuf (@buffer-list nextid)]
              ;(println "nextid" nextid)
                (if-not (nil? anextbuf)
                  (send anextbuf
                        (fn [buf]
                          (-> buf
                              (fire-event :switch-buffer)
                              send-buf!)))))
              (dissoc buf :nextid))))
      (catch Exception e
        (let [output (str e "\n"
                          "stack trace:\n\t"
                          (clojure.string/join "\n\t" (pretty-trace e)) "\n")]
          (-> buf
              (append-output-panel output true)
              (dissoc :nextid)))))))

(comment
  (listen :switch-buffer
          (fn [buf]
            (println "switch buffer to:" (buf :name))
            buf))

  (listen :close-buffer
          (fn [buf]
            (println "close buffer:" (buf :name))
            buf)))

(defn- handle-socket [req]
  {:on-connect (fn [ws]
                 (send ui-agent (fn [ui ws]
                                  (assoc ui :ws ws)) ws))
   :on-text (fn [ws body]
              (let [[id keycode] (parse-input body)]
                (send (@buffer-list id) change-buffer! (input-keys keycode))))})

(defn- write-client! [ui diff]
  (let [ws (ui :ws)]
    (try
      (jetty/send! ws (-> ui :queue (vconj diff) json/generate-string))
      (dissoc ui :queue)
      (catch Exception e
        (update-in ui [:queue] vconj diff)))))

(defonce ^:private web-server (atom nil))

;start app with init file and webserver configs
(defn start [file options]
  (if-not (empty? file) (start-file file))
  (send ui-agent (fn [ui]
                   ;(println "render" (ui :render!))
                   (assoc ui :render! write-client!)))
  (println "start web server:" (options :port))
  (reset! web-server
          (jetty/run-jetty #'app
                           (assoc options :websockets {"/socket" handle-socket}))))

(defn stop []
  (println "stop web server")
  (jetty/stop-server @web-server))

(defn -main [& args]
  (start
    "/tmp/webvim/welcome.txt"
    {:port 8080 :join? true}))
