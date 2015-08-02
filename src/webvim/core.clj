(ns webvim.core
  (:require [ring.adapter.jetty :as jetty]
            [clojure.core.async :as async]
            [ring.util.response :as response])
  (:use clojure.pprint
        webvim.buffer
        webvim.keymap
        webvim.global
        webvim.autocompl
        (compojure handler [core :only (GET POST defroutes)])
        (hiccup [page :only (html5)])
        ring.middleware.resource
        ring.util.response
        ring.middleware.json))

(defn dissoc-emtpy[b ks]
  (if (empty? (get-in b ks))
    (if (= 1 (count ks))
      (dissoc b (first ks))
      (recur (update-in b (pop ks) dissoc (peek ks)) (pop ks)))
    b))

;diff line by line
;TODO more advanced diff algorithm
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
                               (let[]
                                 (autocompl-words-remove l1)
                                 (autocompl-words-parse l2)
                                 l2))) lines1 lines2))))
      :else
      (let[]
        (autocompl-words-remove-buffer before)
        (autocompl-words-parse-buffer after)
        after))))

(defn- remove-server-only-fields[b]
  (-> b 
      (dissoc :history :txt-cache :context :last-cursor 
          :macro :chan-in :chan-out :registers)
      (dissoc-emtpy [:highlights])
      (dissoc-emtpy [:autocompl :suggestions])))

(defn- remove-visual-mode[b]
  (if (empty? (-> b :visual :ranges))
    (dissoc b :visual)
    b))

(defn- dissoc-if-equal[after before k]
  (if (= (before k) (after k))
    (dissoc after k)
    after))

(defn render 
  "Write changes to browser."
  [before after]
  (let [b (cond (= before after)
                ""
                (or (nil? before) (not (= (:id before) (:id after))))
                (remove-server-only-fields after)
                :else
                (-> after
                    remove-visual-mode
                    remove-server-only-fields
                    (diff-lines before)
                    (dissoc-if-equal before :mode)
                    (dissoc-if-equal before :ex)
                    (dissoc-if-equal before :message)))]
    (response b)))


(defn restart-key-server
  "For repl"
  []
  (init-keymap-tree)
  (let [b (active-buffer)
        _ (async/close! (:chan-in b))
        b2 (-> b
               (assoc :chan-in (async/chan))
               (assoc :chan-out (async/chan)))]
    (swap! buffer-list assoc (:id b2) b2)
    (key-server b2 @root-keymap)
    nil))

;TODO How to run code only in repl env? Conditional compile?
;only for testing on repl
(defonce open-test-file
  (reset! active-buffer-id (-> "testfile.clj"
                               open-file
                               buffer-list-save
                               :id)))
(restart-key-server)

(defn edit [keycode]
  (let [before (active-buffer)]
    (async/>!! (:chan-in before) keycode)
    (let [after (async/<!! (:chan-out before))]
      (swap! buffer-list assoc (:id after) after)
      ;Always write (active-buffer) back because active-buffer-id may change by current key
      (render before (active-buffer)))))

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
  (GET "/buf" [] (render nil (active-buffer)))
  (GET "/resize/:w/:h" [w h] 
       (swap! window assoc :viewport {:w (parse-int w) :h (parse-int h)}))
  (GET "/key" {{keycode :code} :params} (edit keycode)))


(def app
  ;(wrap-json-response main-routes))
  (-> (compojure.handler/api main-routes)
      (wrap-json-response)
      (wrap-resource "public")))

(defonce run-jetty (jetty/run-jetty #'app {:port 8080 :join? false}))
