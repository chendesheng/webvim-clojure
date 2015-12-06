(ns user
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [ring.adapter.jetty9 :as jetty])
  (:use webvim.core.buffer
        webvim.core.rope
        webvim.core.register
        webvim.keymap
        webvim.main))

;I don't like include js library directly, but also don't want download it again and again.
(defn- cache-jquery[]
  (let [path "resources/public/jquery.js"]
    (if-not (fs/exists? path)
      (spit path (slurp "http://libs.baidu.com/jquery/2.0.3/jquery.js")))))

(defn restart[]
  (let [keymap (init-keymap-tree)]
    (doseq [abuf (vals @buffer-list)]
      (send abuf #(assoc %1 :root-keymap %2) keymap))
  "ok"))

(defonce ^:private main
  (do
    (cache-jquery)
    (start 
      "testfile.clj"
      {:port 8080 :join? false})))
