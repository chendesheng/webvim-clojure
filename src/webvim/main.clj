(ns webvim.main
  (:require [clojure.string :as str]
            [webvim.server :as server]
            [webvim.panel :refer [append-output-panel]]
            ;[webvim.persistent :refer [recover-buffers start-track]]
            [webvim.core.rope :refer [re-test]]
            [webvim.core.buffer :refer [buf-match-bracket new-file]]
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

(comment defn- change-buffer! [buf keycodes]
         (time
           (try
             (let [oldbuf buf
                   buf (-> buf (apply-keycodes keycodes) send-buf!)
                   nextid (buf :nextid)]
        ;TODO: add event here
               (if (or (not= (oldbuf :str) (buf :str)) (not= (oldbuf :pos) (buf :pos)))
                 (if (= (buf :mode) :insert-mode)
                   (buf-match-bracket buf (-> buf :pos dec))
                   (buf-match-bracket buf)))
               (if (nil? nextid)
                 buf
                 (do
                   (update-buffer nextid
                                  (fn [buf]
                                    (-> buf
                                        (fire-event :switch-buffer)
                                        buf-match-bracket
                                        (send-buf! true))))
                   (dissoc buf :nextid))))
             (catch Exception e
               (fire-event :error (keycode-cancel buf) e)))))

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
