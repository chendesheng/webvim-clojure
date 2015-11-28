(ns user
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [ring.adapter.jetty9 :as jetty])
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
  (let [f "testfile.clj" 
        id (-> f new-file :id)]
    (registers-put!  registers "%" {:str f :id id})))

(defn start[]
  (init-keymap-tree)
  (cache-jquery)
  (open-test-file)
  (run-webserver 8080 false))

(defn restart![]
  (for [buf @buffer-list]
    (-> buf
        (assoc :root-keymap (init-keymap-tree))
        restart-key-server
        save-buffer!))
  "ok")

(defonce ^:private editor
  (start))
