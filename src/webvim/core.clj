(ns webvim.core
  (:require [ring.adapter.jetty :as jetty]
            [clojure.core.async :as async]
            [ring.util.response :as response])
  (:use clojure.pprint
        webvim.buffer
        webvim.keymap
        (compojure handler [core :only (GET POST defroutes)])
        (hiccup [page :only (html5)])
        ring.middleware.resource
        ring.util.response
        ring.middleware.json))

(defn- pprint2[obj prefix]
  (let[]
    (println prefix)
    (pprint obj)
    obj))

(defn dissoc-if-emtpy[b p]
  (if (empty? (b p))
    (dissoc b p)
    b))

(defn render 
  "Write changes to browser."
  ;TODO: only write changed lines
  [before after]
  (let [buf (if (= before after)
              nil
              (if (= (:lines before) (:lines after))
                (dissoc after :lines)
                after))]
    (if (nil? buf)
      (response "")
      (let [b (-> buf 
                  (dissoc :name :history :txt-cache :context :last-cursor :macro :chan-in :chan-out)
                  (dissoc-if-emtpy :highlights))
            b1 (if (-> b :autocompl :suggestions empty?)
                 (dissoc b :autocompl)
                 (update-in b [:autocompl] dissoc :words))]
        (if (not (= visual-mode (:mode b1)))
          (response (dissoc b1 :visual))
          (response b1))))))

(defonce run-key-server 
  (do
    (init-keymap-tree)
    (key-server @active-buffer @root-keymap)))

(defn restart-key-server
  "For repl"
  []
  (let [b @active-buffer]
    (async/close! (:chan-in b))
    (reset! active-buffer 
            (-> b
                (assoc :chan-in (async/chan))
                (assoc :chan-out (async/chan))))
    (init-keymap-tree)
    (key-server @active-buffer @root-keymap)))

(defn edit [keycode]
  (let [before @active-buffer]
    (async/>!! (:chan-in before) keycode)
    (let [after (async/<!! (:chan-out before))]
      (reset! active-buffer after)
      (render before after))))

(defn parse-int [s]
  (Integer. (re-find #"\d+" s)))

(defn homepage
  [request]
  (html5 
    [:head
     [:script {:src "http://libs.baidu.com/jquery/2.0.3/jquery.js" :type "text/javascript"}]
     [:script {:src "keycode.js" :type "text/javascript"}]
     [:script {:src "keyboard.js" :type "text/javascript"}]
     [:script {:src "main.js" :type "text/javascript"}]
     [:link {:href "main.css" :rel "stylesheet"}]]
    [:body
     [:div.gutter]
     [:div.lines]
     [:div.status-bar [:pre]]]))

(defroutes main-routes
  (GET "/" [request] (homepage request))
  (GET "/buf" [] (response (-> @active-buffer 
                               (dissoc :chan-in :chan-out :context 
                                   :history :last-cursor)
                               (dissoc-if-emtpy :highlights))))
  (GET "/resize/:w/:h" [w h] 
       (swap! window assoc :viewport {:w (parse-int w) :h (parse-int h)}))
  (GET "/key" {{keycode :code} :params} (edit keycode)))


(def app
  ;(wrap-json-response main-routes))
  (-> (compojure.handler/api main-routes)
      (wrap-json-response)
      (wrap-resource "public")))

(defonce run-jetty (jetty/run-jetty #'app {:port 8080 :join? false}))
