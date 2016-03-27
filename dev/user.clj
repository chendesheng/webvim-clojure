(ns user
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [cljfmt.core :as cljfmt]
            [webvim.panel :refer [append-output-panel]]
            [clojure.string :as string]
            [webvim.server :as server]
            [webvim.main :refer [start]])
  (:use clojure.pprint
        clojure.repl
        webvim.core.buffer
        webvim.core.ui
        webvim.core.rope
        webvim.core.register
        webvim.core.event
        webvim.core.utils
        webvim.keymap))

(defn print-buf
  ([]
    (let [buf (dissoc (get-from-ui :buf) :str :history :keymap :normal-mode-keymap 
                      :insert-mode-keymap :ex-mode-keymap :context)]
      (pprint buf)))
  ([& ks]
    (pprint  ((get-from-ui :buf) ks))))

(defn- cache-resource [path url]
  (if-not (fs/exists? path)
    (spit path (slurp url))))

;I don't like include js library directly, but also don't want download it over and over.
(defn- cache-resources []
  (doseq [r [["resources/public/jquery.js" "http://libs.baidu.com/jquery/2.0.3/jquery.js"]
             ["resources/public/ubuntu-mono.css" "http://fonts.useso.com/css?family=Ubuntu+Mono"]]]
    (apply cache-resource r)))

(defn restart []
  (do-buffers
    (fn [buf]
      (let [tmp (init-keymap-tree buf)
            keymaps (assoc tmp :keymap (tmp :normal-mode-keymap))]
        (merge buf keymaps))))
  (future
    (Thread/sleep 10) ;wait some time so restart happens after flush states to client
    (server/stop)
    (start nil false {:port 8080 :join? false}))
  "ok")

;FIXME: This is too hacky
(defn- cmd-reload [buf execmd args]
  (let [[[_ _ _ nm]] (re-seq #"(?i)webvim[/\\](src|dev)(/|\\)(.+)\.clj" (buf :filepath)) 
        ret (if (empty? nm)
              "Can't get right namespace"
              (let [code (str "(use '" (-> nm
                                           (string/replace "/" ".")
                                           (string/replace "\\" ".")
                                           (string/replace "_" "-")) " :reload)")]
                (->> code read-string eval)))]
    (if (nil? ret)
      (assoc buf :message (restart))
      (assoc buf :message (str ret)))))

(defn- print-eval [buf code]
  (append-output-panel 
    buf
    (format ":eval %s\n %s" code
            (with-out-str
              (->> code read-string eval str)))
    true))

(defn- cmd-doc [buf execmd args]
  (let [code (str "(clojure.repl/doc " args ")")]
    (print-eval buf code)))

(defn- cmd-print [buf execmd args]
  (let [code (str "(user/print-buf " args ")")]
    (print-eval buf code)))

(defn add-init-ex-commands-event []
  (listen :init-ex-commands
          (fn [cmds _]
            (conj cmds 
                  ["reload" cmd-reload]
                  ["print" cmd-print]
                  ["doc" cmd-doc]))))

(defn- get-files []
  (let [dir? #(.isDirectory %)]
    (tree-seq dir? #(.listFiles %) fs/*cwd*)))

(defn rename [from to]
  (fs/rename from to))

(defn delete [f]
  (fs/delete f))

(defn format-all-js []
  (doseq [f (filter (fn [f]
                      (clojure.string/ends-with? f ".css")) (get-files))]
    (println f)
    (clojure.java.shell/sh "js-beautify" "-r" "-f" (str f))))

(defn- format-clj-file [f]
  (let [s (slurp f)
        news (cljfmt/reformat-string s {:indents (assoc cljfmt/default-indents #".*" [[:block 0]])})]
    (spit f news)))

(defn format-all-clojure []
  (doseq [f (filter (fn [f]
                      (clojure.string/ends-with? f ".clj")) (get-files))]
    (println f)
    (format-clj-file (str f))))

(defn reset-buffers []
  (let [buffers (get-buffers persistent-buffers)]
    (reset-buffers!)
    (registers-put! "%" nil)
    (doseq [{filepath :filepath y :y} buffers]
      (let [buf @(new-file filepath y)]
        (cond
          (and (-> (get-from-ui :buf) :filepath nil? not)
               (path= filepath ((get-from-ui :buf) :filepath)))
          (do
            (update-ui (fn [ui] (dissoc ui :buf)))
            (registers-put! "%" {:str filepath :id (buf :id)})
            (send-buf! buf))
          (path= filepath (:str (registers-get "#")))
          (registers-put! "#" {:str filepath :id (buf :id)}))))
    (if (nil? (registers-get "%"))
      (let [buf (first (get-buffers))]
        (registers-put! "%" {:str (buf :filepath) :id (buf :id)})
        (send-buf! buf)))))

;Not sure why agent await blocking everything. Start a java thread works fine.
(defonce main 
  (do
    (cache-resources)
    (add-init-ex-commands-event)
    (start
      "testfile.clj"
      true
      {:port 8080 :join? false})))
