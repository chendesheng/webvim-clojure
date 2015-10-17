(ns user
  (:require [me.raynes.fs :as fs]
            [ring.adapter.jetty :as jetty]
            [clojure.core.async :as async])
  (:use webvim.core.buffer
        webvim.core.rope
        webvim.core.register
        webvim.keymap
        webvim.main
        webvim.core.serve))

(defn- restart-key-server
  [buf]
  (let [_ (async/close! (:chan-in buf))
        buf1 (-> buf
               (assoc :chan-in (async/chan))
               (assoc :chan-out (async/chan)))]
    (key-server buf1)))

;I don't like include js library directly, but also don't want download it again and again.
(defn- cache-jquery[]
  (let [path "resources/public/jquery.js"]
    (if-not (fs/exists? path)
      (spit path (slurp "http://libs.baidu.com/jquery/2.0.3/jquery.js")))))

(defn- open-test-file[]
  (let [id (reset! active-buffer-id 
                   (-> "testfile.clj"
                       new-file
                       :id))]
    (registers-put!
      registers
      "%" 
      {:str "testfile.clj" :id id})))

(defn start[]
  (init-keymap-tree)
  (cache-jquery)
  (open-test-file)
  (jetty/run-jetty #'app {:port 8080 :join? false}))

(defn- set-buffer![buf]
  (swap! buffer-list assoc (:id buf) buf))

(defn restart[]
  (-> (active-buffer)
      (assoc :root-keymap (init-keymap-tree))
      restart-key-server
      set-buffer!)
  "ok")

(defonce ^:private editor
  (start))
