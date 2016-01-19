(ns webvim.keymap.external
  (:require [clojure.core.async :as async])
  (:use webvim.core.utils
        webvim.core.rope
        webvim.core.buffer
        webvim.core.pos
        webvim.keymap.action
        webvim.jumplist))

(defn- readonly[buf]
  (assoc buf :message "Sorry, it's readonly."))

;for buffer generate by external commands like grep, make etc
(defn init-external-output-keymap[motion-keymap]
  (assoc
    motion-keymap
    "i" readonly
    "c" readonly
    "a" readonly
    "s" readonly
    "x" readonly
    "d" readonly
    "<c-o>" #(move-to-jumplist % jump-prev)
    "<c-i>" #(move-to-jumplist % jump-next)))
