(ns webvim.keymap.external
  (:require [clojure.core.async :as async])
  (:use webvim.utils
        webvim.core.rope
        webvim.core.buffer
        webvim.core.pos
        webvim.jumplist))

(defn- readonly[buf]
  (assoc buf :message "Sorry, it's readonly."))

;for buffer generate by external commands like grep, make etc
(defn init-external-output-keymap[motion-keymap]
  (deep-merge
    motion-keymap
    {"<append>" ;this particular keycode is only meant for appending text to current buffer, any thing after this keycode is append to the end of :str.
     {:enter (fn[buf keycode]
               (dissoc buf :keys))
      :else (fn[buf stream]
              (-> buf
                  buf-end
                  (buf-insert stream)
                  buf-end))
      }
     "<c+c>" (fn[buf]
               (async/close! (buf :chan-in))
               buf)
     "i" readonly
     "c" readonly
     "a" readonly
     "s" readonly
     "x" readonly
     "d" readonly}))
