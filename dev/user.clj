(ns user
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [ring.adapter.jetty9 :as jetty]
            [clojure.string :as string])
  (:use clojure.pprint
        clojure.repl
        webvim.core.buffer
        webvim.core.ui
        webvim.core.rope
        webvim.core.register
        webvim.core.event
        webvim.core.utils
        webvim.keymap
        webvim.keymap.action
        webvim.main))

(defn print-buf
  ([]
    (let [buf (-> @ui-agent :buf 
                  (dissoc :str :history :keymap :normal-mode-keymap 
                          :insert-mode-keymap :ex-mode-keymap :context))]
      (pprint buf)))
  ([& ks]
     (pprint (-> @ui-agent :buf (get-in ks)))))

(defn- cache-resource[path url]
  (if-not (fs/exists? path)
    (spit path (slurp url))))

;I don't like include js library directly, but also don't want download it over and over.
(defn- cache-resources[]
  (doseq [r [["resources/public/jquery.js" "http://libs.baidu.com/jquery/2.0.3/jquery.js"]
             ["resources/public/ubuntu-mono.css" "http://fonts.useso.com/css?family=Ubuntu+Mono"]]]
    (apply cache-resource r)))

(defn restart[]
  (let [tmp (init-keymap-tree)
        keymaps (assoc tmp :keymap (tmp :normal-mode-keymap))]
    (send ui-agent (fn[ui] (assoc ui :keymaps tmp)))
    (doseq [abuf (vals @buffer-list)]
      (send abuf (fn[buf]
                   (merge buf keymaps)))))
  (future
    (Thread/sleep 10) ;wait some time so restart happens after flush states to client
    (stop)
    (start nil {:port 8080 :join? false}))
  "ok")

;FIXME: This is too hacky
(defn- cmd-reload[buf execmd args]
  (let [[[_ _ nm]] (re-seq #"(?i)^(src|dev)/(.+)\.clj" (-> buf :filepath shorten-path)) 
        ret (if (empty? nm)
              "Can't get right namespace"
              (let [code (str "(use '" (-> nm
                                           (string/replace "/" ".")
                                           (string/replace "_" "-")) " :reload)")]
                (->> code read-string eval)))]
    (if (nil? ret)
      (assoc buf :message (restart))
      (assoc buf :message (str ret)))))

(defn- print-eval[buf code]
  (append-output-panel 
    buf
    (with-out-str
      (->> code read-string eval str))
    true))

(defn- cmd-doc[buf execmd args]
  (let [code (str "(clojure.repl/doc " args ")")]
    (print-eval buf code)))

(defn- cmd-print[buf execmd args]
  (let [code (str "(user/print-buf " args ")")]
    (print-eval buf code)))

(defn add-init-ex-commands-event[]
  (listen :init-ex-commands
          (fn[cmds]
            (conj cmds 
                  ["reload" cmd-reload]
                  ["print" cmd-print]
                  ["doc" cmd-doc]))))

;Not sure why agent await blocking everything. Start a java thread works fine.
(defonce main 
  (do
    (cache-resources)
    (add-init-ex-commands-event)
    (start
      "testfile.clj"
      {:port 8080 :join? false})))
