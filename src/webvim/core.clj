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
      (let [b (dissoc buf :viewport :name :history)]
        (if (not (= 3 (:mode b)))
          (response (dissoc b :visual))
          (response b))))))

(defn serve-keys
  "Serve a sequence of keys until end of keymap. (works like sytax parser)"
  [in out b keymap]
  (let [keycode (async/<!! in)
        aa (println "got key:" keycode)
        f (keymap keycode)]
    (println f)
    (cond
;      (= keycode "c+c") ;;stop everything and get back to normal mode
;      (let [b1 (set-normal-mode @b)]
;        (async/>!! out b1reset! b b1))

      (map? f)
      (let [b1 (swap! b buffer-append-keys keycode)]
        (async/>!! out b1)
        (serve-keys in out b f))

      (fn? f)
      (let [b1 (dissoc (f @b) :keys)]
        (async/>!! out (reset! b b1)))

      (nil? f)
      (async/>!! out (let [else (:else keymap)]
                       (if (nil? else)
                         (reset! b @b)
                         (reset! b (else @b keycode))))))))

(defonce key-server-in (async/chan))
(defonce key-server-out (async/chan))

(defn key-server[]
  "Start a dedicate thread handle input keys, this thread should never stop util application ends."
  (async/thread 
    (loop[]
      (try
        (serve-keys key-server-in key-server-out active-buffer @active-keymap)
        (catch Exception e
          (let [err (str "caught exception: " (.getMessage e))]
            (println (.getMessage e))
            (.printStackTrace e)
            (async/>!! key-server-out (swap! active-buffer merge {:ex "" :mode 0 :message err})))))
      (recur))))

(defonce run-key-server 
  (do
    (init-keymaps)
    (key-server)))

(defn edit [keycode]
  (let [before @active-buffer]
    (async/>!! key-server-in keycode)
    (let [after (async/<!! key-server-out)]
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
       (swap! active-buffer assoc :viewport {:w (parse-int w) :h (parse-int h)}))
  (GET "/key/:keycode" [keycode] (edit keycode)))


(def app
  ;(wrap-json-response main-routes))
  (-> (compojure.handler/site main-routes)
      (wrap-json-response)
      (wrap-resource "public")))

(defonce run-jetty (jetty/run-jetty #'app {:port 8080 :join? false}))
