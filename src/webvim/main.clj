(ns webvim.main
  (:require [clojure.string :as str]
            [webvim.server :as server]
            [webvim.panel :refer [append-output-panel]]
            ;[webvim.persistent :refer [recover-buffers start-track]]
            [webvim.core.rope :refer [re-test]]
            [webvim.keymap.compile :refer [keycode-cancel apply-keycodes]]
            [webvim.core.event :refer [listen fire-event]]
            [webvim.core.editor :refer [update-buffer]])
  (:use clojure.pprint
        webvim.lang.clojure ;TODO: load language setting dynamically
        webvim.lang.lisp
        webvim.lang.javascript
        webvim.lang.css
        webvim.lang.sql
        webvim.lang.go
        webvim.lang.csharp
        webvim.lang.html))

(defn- apply-keycodes-transact [buf keycodes]
  (time
    (try
      (apply-keycodes buf keycodes)
      (catch Exception e
        (fire-event :error (keycode-cancel buf) e)))))

(listen :input-keys
        (fn [{awindow :awindow bufid :bufid keycodes :keycodes}]
          (send awindow
                (fn [old-window bufid keycodes]
                  ;(println "input-keys:" keycodes)
                  (update-buffer old-window
                                 bufid
                                 #(apply-keycodes-transact % keycodes)))
                bufid keycodes)))

;start app with init file and webserver configs
(defn start [recover-buffers? options]
  ;FIXME: add back later 
  ;(if recover-buffers? (recover-buffers))
  ;(start-track)
  (println "start web server:" (options :port))
  (server/run options))

(defn -main [& args]
  (start
    true
    {:port 8080 :join? true}))
