(ns webvim.mode
  (:require [webvim.core.event :refer [fire-event]]))

(defn set-insert-mode [buf]
  (assoc buf
         :mode :insert-mode
         :keymap (buf :insert-mode-keymap)))

(defn- fire-change-to-normal-mode-event [buf]
  (if (not= (buf :mode) :normal-mode)
    (fire-event buf :before-change-to-normal-mode)
    buf))

(defn set-normal-mode [buf]
  ;(println "set-normal-mode:")
  (-> buf
      fire-change-to-normal-mode-event
      (assoc 
        :mode :normal-mode
        :keymap (buf :normal-mode-keymap))))

