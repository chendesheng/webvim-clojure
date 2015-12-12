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
        webvim.keymap.action
        webvim.main))

(defn- cache-resource[path url]
  (if-not (fs/exists? path)
    (spit path (slurp url))))

;I don't like include js library directly, but also don't want download it again and again.
(defn- cache-resources[]
  (let [resources
        [["resources/public/jquery.js" "http://libs.baidu.com/jquery/2.0.3/jquery.js"]
         ["resources/public/ubuntu-mono.css" "http://fonts.useso.com/css?family=Ubuntu+Mono"]]]
    (doseq [r resources]
      (apply cache-resource r))))

(defn restart[]
  (let [keymap @root-keymap]
    (doseq [abuf (vals @buffer-list)]
      (send abuf #(assoc %1 :root-keymap %2) keymap))
    (future
      (Thread/sleep 10) ;wait some time so restart happens after flush states to client
      (stop)
      (start nil {:port 8080 :join? false}))
    "ok"))

;FIXME: This is too hacky
(defn- cmd-reload[buf execmd args]
  (let [[[_ _ nm]] (re-seq #"(src|dev)/(.+)\.clj" (buf :filepath)) 
        code (str "(use '" (-> nm
                               (string/replace "/" ".")
                               (string/replace "_" "-")) " :reload)")
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
    (cache-resources)
    (add-init-ex-commands-event)
    (start
      "testfile.clj"
      {:port 8080 :join? false})))
