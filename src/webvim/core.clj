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
      (let [b (dissoc buf :name :history :txt-cache :context :last-cursor)
            b1 (if (-> b :autocompl :suggestions empty?)
                 (dissoc b :autocompl)
                 (update-in b [:autocompl] dissoc :words))]
        (if (not (= visual-mode (:mode b1)))
          (response (dissoc b1 :visual))
          (response b1))))))

(defn key-server[]
  "Start a dedicate thread handle input keys, this thread should never stop util application ends."
  (async/thread 
    (loop[]
      (try
        (let [keycode (async/<!! key-server-in)]
          (async/>!! key-server-out 
                     (reset! active-buffer 
                             (-> @active-buffer
                                 (serve-keys key-server-in key-server-out @root-keymap keycode)
                                 (buffer-reset-keys)))))
        (catch Exception e
          (let [err (str "caught exception: " e)]
            (println err)
            (.printStackTrace e)
            (reset! root-keymap @normal-mode-keymap)
            (async/>!! key-server-out (swap! active-buffer merge {:ex "" :mode 0 :message err})))))
      (recur))))

(defonce run-key-server 
  (do
    (init-keymap-tree)
    (key-server)))

(defn edit [keycode]
  (let [before @active-buffer]
    (async/>!! key-server-in keycode)
    (let [after (async/<!! key-server-out)]
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
  (GET "/buf" [] (response @active-buffer))
  (GET "/resize/:w/:h" [w h] 
       (swap! window assoc :viewport {:w (parse-int w) :h (parse-int h)}))
  (GET "/key/:keycode" [keycode] (edit keycode)))


(def app
  ;(wrap-json-response main-routes))
  (-> (compojure.handler/site main-routes)
      (wrap-json-response)
      (wrap-resource "public")))

(defonce run-jetty (jetty/run-jetty #'app {:port 8080 :join? false}))
