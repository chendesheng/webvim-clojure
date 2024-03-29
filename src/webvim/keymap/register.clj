(ns webvim.keymap.register
  (:require [webvim.core.eval :refer [eval-sandbox]])
  (:use clojure.pprint
        webvim.keymap.compile
        webvim.keymap.linebuf.linebuf
        webvim.core.ui
        webvim.core.utils
        webvim.core.rope
        webvim.core.register
        webvim.core.buffer
        webvim.core.event))

(defn- start-register [buf keycode]
  (if (re-test #"[0-9a-zA-Z/*#%.:+=-_~]" keycode)
    (assoc-in buf [:context :register] keycode)
    buf))

(defn- read-eval-put [buf code insert?]
  (let [{result :result
         exception :exception} (eval-sandbox code)]
    (if (nil? exception)
      (let [buf (-> buf
                    (update-in [:window :registers] registers-put "=" {:str code :result result})
                    (assoc-in [:context :register] "="))]
        (if insert?
          (buf-insert result) buf))
      (assoc buf :message (str exception)))))

(defonce ^:private linebuf-keymap (init-linebuf-keymap))
(defn- expression-keymap [insert?]
  (-> linebuf-keymap
      (wrap-key
        :enter (fn [handler]
                 (fn [buf keycode]
                   (-> buf
                       (assoc :line-buffer {:prefix keycode :str (rope "()") :pos 1})
                       (handler keycode)))))
      (assoc "<cr>" (fn [buf keycode]
                      (let [code (-> buf :line-buffer :str str)]
                        (read-eval-put buf code insert?))))))

(defn- on-normal-mode-keymap [keymap]
  (-> keymap
      (wrap-key :before
                (fn [handler]
                  (fn [buf keycode]
                    (-> buf
                        (update-in [:context :register]
                                   (fn [reg]
                                     (or reg "\"")))
                        (handler keycode)))))
      (assoc "\"" {"<esc>" nop
                   "=" (expression-keymap false)
                   :else start-register})))

(listen
  :normal-mode-keymap
  (fn [keymap _]
    (on-normal-mode-keymap keymap)))

(listen
  :insert-mode-keymap
  (fn [keymap _]
    (update keymap "<c-r>" assoc "=" (expression-keymap true))))

(listen
  :visual-mode-keymap
  (fn [keymap _]
    (on-normal-mode-keymap keymap)))

