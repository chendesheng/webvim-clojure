(ns user
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [ring.adapter.jetty9 :as jetty]
            [clojure.string :as string])
  (:use webvim.core.buffer
        webvim.core.rope
        webvim.core.register
        webvim.core.event
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

(defn- cmd-reload[buf execmd args]
  (let [[[_ nm]] (re-seq #"src/(.+)\.clj" (buf :filepath))
        code (str "(use '" (string/replace nm "/" ".") " :reload)")
        ret (->> code read-string eval)]
    (if (nil? ret)
      (assoc buf :message (restart))
      (assoc buf :message (str ret)))))

(defn add-init-ex-commands-event[]
  (listen :init-ex-commands
          (fn[cmds]
            (conj cmds ["reload" cmd-reload]))))

(defonce ^:private main
  (do
    (cache-jquery)
    (add-init-ex-commands-event)
    (start
      "testfile.clj"
      {:port 8080 :join? false})))
