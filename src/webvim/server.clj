(ns webvim.server
  (:require [hiccup.page :refer [html5]]
            [ring.util.response :as response]
            [cheshire.core :as json]
            [org.httpkit.server :refer [run-server with-channel on-receive on-close send! close]]
            [webvim.core.editor :refer [*window* get-or-create-window with-window with-window-id]]
            [webvim.core.ui :refer [update-ui get-from-ui ui-buf]]
            [webvim.core.utils :refer [vconj parse-int]]
            [webvim.core.event :refer [fire-event]])
  (:use (compojure handler [core :only (GET POST defroutes)]) 
        ring.middleware.resource
        ring.util.response
        ring.middleware.json
        ring.middleware.content-type))

(defn- add-js [node src]
  (conj node [:script {:src src :type "text/javascript"}]))

(defn- add-css [node href]
  (conj node [:link {:href href :rel "stylesheet"}]))

(defn- homepage
  [request]
  (let [js ["jquery.js" "cookie.js" "socket.js" "utils.js" "dom.js" "keycode.js" "keymap.js"
            "keyboard.js" "syntax/clojure.js" "syntax/css.js" "syntax/xml.js" 
            "syntax/sql.js" "syntax/go.js" "syntax/cs.js" "syntax/javascript.js" 
            "highlight.js" "main.js" "render/autocompl.js" "render/cursor.js" 
            "render/gutter.js" "render/offscreen/changes.js" "render/offscreen/lines.js" 
            "render/offscreen/pos.js" "render/selection.js" "render/viewport.js" 
            "render/watchers.js"]
        css ["main.css" "monokai.css"]]
    (html5
      (reduce add-js [:head] js)
      (reduce add-css [:head] css)
      [:body])))

(defn- parse-input [body]
  (let [[id keycode]
        (-> #"(?s)(\d+)\!(.*)"
            (re-seq body)
            first
            rest)]
    [(Integer. id) keycode]))

(defn- write-client! [ui diff]
  (let [ws (ui :ws)]
    (try
      (send! ws (-> ui :queue (vconj diff) json/generate-string))
      (dissoc ui :queue)
      (catch Exception e
        (fire-event e :exception)
        (update ui :queue vconj diff)))))

(defn- handle-socket [request]
  (let [channels (atom {})]
    (with-channel
      request channel
      (on-receive channel
                  (fn [body]
                    (with-window (@channels channel)
                                 (fire-event (parse-input body) :input-keys))))
      (on-close channel
                (fn [status] (println "websocket close")))
      ;setup window context
      (let [window (get-or-create-window
                     (-> request :query-params (get "windowId")))]
        ;(println window)
        (with-window
          window
          ;close last channel
          (if-let [ws (get-from-ui :ws)]
            (do
              (close ws)
              (swap! channels dissoc ws)))
          (swap! channels assoc channel window)
          ;set current channel
          (update-ui
            (fn [ui ws]
              (assoc ui
                     :ws ws
                     :render! write-client!)) channel)
          (if (-> request :query-params (contains? "init"))
            (send! channel (json/generate-string [(ui-buf)]))))))))

(defonce ^:private web-server (atom nil))

(defn stop []
  (println "stop web server")
  (@web-server))

(defroutes main-routes
  (GET "/" [request] (homepage request))
  (GET "/socket" [] handle-socket)
  ;FIXME: add back later
  (GET "/resize/:id/:w/:h" [id w h]
    (with-window-id id
                    (update-ui
                      (fn [ui w h]
                        (update ui :viewport assoc :w w :h h)) (parse-int w) (parse-int h)))))

(def ^:private app
  (-> (compojure.handler/api main-routes)
      (wrap-json-response)
      (wrap-resource "public")
      (wrap-content-type)))

(defn run [options]
  (reset! web-server
          (run-server #'app options)))


