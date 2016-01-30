(ns webvim.keymap.dotrepeat
  (:require [me.raynes.fs :as fs]
            [clojure.string :as string])
  (:use clojure.pprint
        webvim.keymap.action
        webvim.core.ui
        webvim.core.buffer
        webvim.core.register
        webvim.core.event))

(defn- dot-repeat[buf]
  (let [keycodes ((registers-get ".") :keys)]
    (if (empty? keycodes)
      buf
      (replay-keys buf keycodes))))

(defn- save-dot-repeat[buf]
  (let [keys (-> buf :dot-repeat-keys reverse)
        nochange? (-> buf :pending-undo empty?)]
    (if-not (or nochange? ;only repeat keys make changes
                (empty? keys)
                ;don't repeat these keys
                (contains? #{"." "u" "p" "P" ":" "<c-r>"} (first keys)))
      (registers-put! "." {:keys keys :str (string/join keys)}))
    (dissoc buf :dot-repeat-keys)))

(defn- on-before-handle-key[buf keycode]
  (update-in buf [:dot-repeat-keys] conj keycode))

(defn- on-normal-mode-keymap[keymap]
  (-> keymap
      (key-do-before 
        :after
        (fn[buf keycode]
          (if (= (buf :mode) insert-mode)
            buf
            (save-dot-repeat buf))))
      (assoc "." dot-repeat)))

(defn- on-insert-mode-keymap[keymap]
  (key-do-before
    keymap :leave
    (fn[buf keycode]
      (save-dot-repeat buf))))

(defonce ^:private listener1
  (listen
    :before-handle-key
    (fn[buf keycode]
      (on-before-handle-key buf keycode))))

(defonce ^:private listener2
  (listen
    :normal-mode-keymap
    (fn[keymap]
      (on-normal-mode-keymap keymap))))

(defonce ^:private listener3
  (listen
    :insert-mode-keymap
    (fn[keymap]
      (on-insert-mode-keymap keymap))))



