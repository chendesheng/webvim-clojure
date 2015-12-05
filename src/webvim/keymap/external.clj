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
  (merge
    motion-keymap
    {"i" readonly
     "c" readonly
     "a" readonly
     "s" readonly
     "x" readonly
     "d" readonly}))
