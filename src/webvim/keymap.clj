(ns webvim.keymap
  (:require [clojure.core.async :as async])
  (:use clojure.pprint
        webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.buffer
        webvim.core.serve
        webvim.core.line
        webvim.keymap.motion
        webvim.keymap.visual
        webvim.keymap.normal
        webvim.keymap.insert
        webvim.keymap.ex
        webvim.keymap.line-editor
        webvim.keymap.pair
        webvim.keymap.action
        webvim.core.register
        webvim.jumplist
        webvim.utils
        webvim.indent
        webvim.autocompl))

(defn init-keymap-tree
  []
  (let [insert-mode-keymap (init-insert-mode-keymap)
        pair-keymap (init-pair-keymap)
        line-editor-keymap (init-line-editor-keymap)
        motion-keymap (init-motion-keymap line-editor-keymap)
        ex-mode-keymap (init-ex-mode-keymap motion-keymap line-editor-keymap)
        visual-mode-keymap (init-visual-mode-keymap motion-keymap pair-keymap visual-normal)
        visual-line-mode-keymap (init-visual-mode-keymap motion-keymap pair-keymap visual-line)
        normal-mode-keymap (init-normal-mode-keymap motion-keymap insert-mode-keymap visual-mode-keymap visual-line-mode-keymap ex-mode-keymap pair-keymap)]
    (reset! root-keymap normal-mode-keymap)))

(defn- buf-bound-scroll-top
  "Change scroll top make cursor inside viewport"
  [buf]
  (let [st (buf :scroll-top)]
    (assoc buf :scroll-top 
           (let [y (buf :y)
                 h (-> @window :viewport :h)]
             (cond 
               (< y st) y
               (< y (+ st h)) st
               (neg? (-> y (- h) inc)) 0
               :else (-> y (- h) inc))))))

(defonce ^:private listen-new-buffer
  (listen :new-buffer
          (fn [buf]
            (-> buf
                (assoc :before-send-out buf-bound-scroll-top)
                (assoc :after-send-out #(-> %
                                            (assoc :changes [])
                                            (assoc :nextid nil)))))))
