(ns webvim.server
  (:require [hiccup.page :refer [html5]]
            [ring.util.response :as response]
            [cheshire.core :as json]
            [org.httpkit.server :refer [run-server with-channel on-receive on-close send! close]]
            [webvim.core.editor :refer [get-or-create-window]]
            [webvim.core.utils :refer [vconj parse-int]]
            [webvim.core.keys :refer [input-keys]]
            [webvim.core.event :refer [fire-event]]
            [webvim.core.ui :refer [diff-buf]])
  (:use (compojure handler [core :only (GET POST defroutes)])
        ring.middleware.resource
        ring.util.response
        ring.middleware.json
        ring.middleware.content-type))

(defn- homepage
  [request]
  (html5
    [:head
     [:script {:src "js/highlight.pack.js"} :type "text/javascript"]
     [:script {:src "js/cljs.js"} :type "text/javascript"]
     [:link {:href "css/cljs.css" :rel "stylesheet"}]
     [:link {:href "css/atom-one-dark.css" :rel "stylesheet"}]
     [:meta {:name "apple-touch-fullscreen" :content "yes"}]
     [:meta {:name "apple-mobile-web-app-capable" :content "yes"}]
     [:meta {:name "apple-mobile-web-app-status-bar-style" :content "default"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]]
    [:body
     [:textarea {:id "hidden-input" :autocomplete "off"
                 :autocorrect "off" :autocapitalize "off"
                 :spellcheck "false" :aria-multiline "true"
                 :role "textbox" :wrap "off" :disabled "disabled"}]
     [:div#editor
      [:div#buffers]
      [:div#autocompl.autocompl]]]))

(defn- parse-input [body]
  (let [[id keycode]
        (-> #"(?s)(\d+)\!(.*)"
            (re-seq body)
            first
            rest)]
    {:bufid (Integer. id)
     :keycodes (input-keys keycode)}))

(defn- write-client! [channel window diff]
  (try
    (send! channel (-> window :queue (vconj diff) json/generate-string))
    (dissoc window :queue)
    (catch Exception e
      (fire-event e :exception)
      (update window :queue vconj diff))))

(defn- window-initial-response [window]
  (-> window
      (select-keys [:id :active-buffer :views :cwd :buffers])
      ; only resonse buffers which attached to a view
      (update :buffers
              (fn [buffers]
                (into {}
                      (->> buffers
                           (filter (fn [[_ buf]]
                                     (-> buf :view some?)))
                           (map (fn [[bufid buf]]
                                  [bufid (diff-buf nil buf)]))))))))

(defn- handle-socket [request]
  (let [channels (atom {})]
    (with-channel
      request channel
      (on-receive channel
                  (fn [body]
                    (println "body:" body)
                    (if-not (empty? body)
                      (-> body
                          parse-input
                          (assoc :awindow (@channels channel))
                          (fire-event :input-keys)))))
      (on-close channel
                (fn [status] (println "websocket close")))

      ;initialize
      (let [awindow (get-or-create-window
                      (-> request :query-params (get "windowId")))
            {old-channel :channel :as window} @awindow]
        (println "input windowid:" (-> request :query-params (get "windowId")))
        (when (some? old-channel)
          (close old-channel)
          (swap! channels dissoc old-channel))
        (swap! channels assoc channel awindow)
        (send awindow
              (fn [window channel]
                (assoc window
                       :channel channel
                       :render! (partial write-client! channel))) channel)
        ;send back full state at start connet
        (if (-> request :query-params (contains? "init"))
          (do
            (println (window-initial-response window))
            (send! channel (json/generate-string (window-initial-response window)))))))))

(defonce ^:private web-server (atom nil))

(defn stop []
  (println "stop web server")
  (@web-server))

(defroutes main-routes
  (GET "/" request (homepage request))
  (GET "/socket" [] handle-socket)
  (GET "/resize/:id/:w/:h" [id w h]
    (send (get-or-create-window id)
          (fn [window w h]
            (update window :viewport assoc :w w :h h))
          (parse-int w) (parse-int h))))

(def ^:private app
  (-> (compojure.handler/api main-routes)
      (wrap-json-response)
      (wrap-resource "public")
      (wrap-content-type)))

(defn run [options]
  (reset! web-server
          (run-server #'app options)))
