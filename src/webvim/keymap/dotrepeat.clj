(ns webvim.keymap.dotrepeat
  (:require [me.raynes.fs :as fs]
            [clojure.string :as string])
  (:use clojure.pprint
        webvim.keymap.compile
        webvim.core.ui
        webvim.core.buffer
        webvim.core.register
        webvim.core.event))

(defn- dot-repeat [buf keycode]
  (let [keycodes (-> buf :window :registers (registers-get ".") :keys)]
    (if (empty? keycodes)
      buf
      (replay-keys buf keycodes))))

(defn- save-dot-repeat [buf]
  (let [keys (-> buf :dot-repeat-keys reverse)
        nochange? (-> buf :pending-undo empty?)
        buf (dissoc buf :dot-repeat-keys)]
    (if-not (or nochange? ;only repeat keys make changes
                (empty? keys)
                ;don't repeat these keys
                (contains? #{"." "u" "p" "P" ":" "<c-r>"} (first keys)))
      (update-in buf
                 [:window :registers]
                 registers-put
                 "."
                 {:keys keys :str (string/join keys)})
      buf)))

(listen
  :before-handle-key
  (fn [buf keycode]
    (update buf :dot-repeat-keys conj keycode)))

(listen
  :normal-mode-keymap
  (fn [keymap _]
    (-> keymap
        (wrap-key
          :after
          (fn [handler]
            (fn [buf keycode]
              (if (= (buf :mode) :insert-mode)
                (handler buf keycode)
                (-> buf
                    save-dot-repeat
                    (handler keycode))))))
        (assoc "." dot-repeat))))

(listen
  :insert-mode-keymap
  (fn [keymap _]
    (wrap-key
      keymap :leave
      (fn [handler]
        (fn [buf keycode]
          (-> buf
              save-dot-repeat
              (handler keycode)))))))



