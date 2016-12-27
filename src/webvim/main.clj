(ns webvim.main
  (:require [clojure.string :as str]
            [webvim.server :as server]
            [webvim.panel :refer [append-output-panel]]
            [webvim.persistent :refer [recover-buffers start-track]]
            [webvim.core.rope :refer [re-test]]
            [webvim.core.ui :refer [send-buf!]]
            [webvim.core.buffer :refer [buf-match-bracket update-buffer new-file]]
            [webvim.core.register :refer [registers-put!]]
            [webvim.keymap.compile :refer [keycode-cancel apply-keycodes]]
            [webvim.core.event :refer [listen fire-event]]
            [webvim.core.keys :refer [input-keys]])
  (:use clojure.pprint
        webvim.lang.clojure ;TODO: load language setting dynamically
        webvim.lang.lisp
        webvim.lang.javascript
        webvim.lang.css
        webvim.lang.sql
        webvim.lang.go
        webvim.lang.csharp
        webvim.lang.html))

(defn- change-buffer! [buf keycodes]
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

(listen :input-keys
        (fn [[id keycode]]
          (update-buffer id change-buffer! (input-keys keycode))))

;start app with init file and webserver configs
(defn start [recover-buffers? options]
  ;FIXME: add back later 
  ;(if recover-buffers? (recover-buffers))
  (start-track)
  (println "start web server:" (options :port))
  (server/run options))

(defn -main [& args]
  (start
    true
    {:port 8080 :join? true}))
