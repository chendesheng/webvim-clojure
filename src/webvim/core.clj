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

(defn dissoc-emtpy[b ks]
  (if (empty? (get-in b ks))
    (if (= 1 (count ks))
      (dissoc b (first ks))
      (recur (update-in b (pop ks) dissoc (peek ks)) (pop ks)))
    b))

;diff line by line
(defn diff-lines [after before]
  (let [lines1 (:lines before)
        lines2 (:lines after)]
    (cond 
      (= lines1 lines2)
      (dissoc after :lines)
      (= (count lines1) (count lines2))
      (-> after
          (dissoc :lines)
          (assoc :difflines 
                 (vec (map (fn[l1 l2]
                             (if (= l1 l2)
                               nil
                               l2)) lines1 lines2))))
      :else
      after)))

(defn- remove-server-only-fields[b]
  (-> b 
      (dissoc :history :txt-cache :context :last-cursor 
          :macro :chan-in :chan-out :registers)
      (dissoc-emtpy [:highlights])
      (update-in [:autocompl] dissoc :words)
      (dissoc-emtpy [:autocompl :suggestions])))

(defn- remove-visual-mode[b]
  (if (not (= visual-mode (:mode b)))
    (dissoc b :visual)
    b))


(defn render 
  "Write changes to browser."
  [before after]
  (let [b (cond (= before after)
                ""
                (or (nil? before) (not (= (:id before) (:id after))))
                (remove-server-only-fields after)
                :else
                (-> after
                    (diff-lines before)
                    remove-server-only-fields
                    remove-visual-mode))]
    (response b)))

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

(restart-key-server)

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
  (GET "/buf" [] (render nil @active-buffer))
  (GET "/resize/:w/:h" [w h] 
       (swap! window assoc :viewport {:w (parse-int w) :h (parse-int h)}))
  (GET "/key" {{keycode :code} :params} (edit keycode)))


(def app
  ;(wrap-json-response main-routes))
  (-> (compojure.handler/api main-routes)
      (wrap-json-response)
      (wrap-resource "public")))

(defonce run-jetty (jetty/run-jetty #'app {:port 8080 :join? false}))
