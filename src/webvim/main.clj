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
        webvim.lang.javascript
        webvim.lang.css
        webvim.lang.sql
        webvim.lang.go
        webvim.lang.csharp
        webvim.lang.html))

(defn- pretty-trace
  "convert error trace to file path with line number"
  [err]
  (let [st (-> err .getStackTrace seq)]
    (map (fn [line]
           (let [class (.getClassName line)
                ;method (.getMethodName line)
                 file (.getFileName line)
                 linenum (.getLineNumber line)
                 appendsrc (fn [name]
                             (if (re-test #"^webvim" name)
                               (str "src/" name)
                               name))]
             (str (-> class
                      (str/replace #"\$.*" "")
                      ;FIXME: handle class name doesn't contains dot 
                      (str/replace #"[.][^.]*$" "")
                      (str/replace "." "/")
                      (str/replace "_" "-")
                      appendsrc) "/" file ":" linenum))) st)))

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
                                 send-buf!)))
            (dissoc buf :nextid))))
      (catch Exception e
        (let [output (str e "\nstack trace:\n\t"
                          (str/join "\n\t" (pretty-trace e)) "\n")]
          (-> buf
              keycode-cancel
              (append-output-panel output true)
              (dissoc :nextid)))))))

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
