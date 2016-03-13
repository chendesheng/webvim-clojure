(ns webvim.keymap.insert
  (:require [webvim.keymap.put :refer [wrap-keymap-put-insert]]
            [webvim.mode :refer [set-normal-mode temp-normal-mode normal-mode insert-mode]])
  (:use webvim.keymap.compile
        webvim.core.buffer
        webvim.core.event
        webvim.core.rope
        webvim.core.line
        webvim.core.pos
        webvim.core.register
        webvim.core.lang
        webvim.indent
        webvim.core.utils
        webvim.jumplist))

(defn- insert-keycode [{pos :pos :as buf} keycode]
  (if (= "<bs>" keycode)
    (if (zero? pos) buf (buf-delete buf (dec pos) pos))
    (buf-insert buf (keycode-to-char keycode))))

(defn- insert-mode-default [buf keycode]
  (let [pos (buf :pos)
        buf (insert-keycode buf keycode)]
    (if (or (indent-trigger? (buf :language) keycode) (= keycode "<cr>"))
      (buf-indent-current-line buf)
      buf)))

(defn- cancel-last-indents [buf]
  (dissoc (if (-> buf :last-indents empty?)
            buf
            (reduce
              (fn [buf [a b]]
                ;(println "cancel-last-indent:" a b)
                (let [r (buf :str)
                      [a1 b1] (pos-line r a)
                      s (subr r a1 b1)]
                  (if (and (= a1 a)
                           (<= b1 b)
                           (rblank? s))
                    (buf-delete buf a1 (dec b1)) buf))) buf (buf :last-indents))) :last-indents))

(defn- temp-normal-mode-keymap [normal-mode-keymap]
  (-> normal-mode-keymap
      (dissoc "u" "<c-r>") ;can't make undo/redo in the middle of change
      (assoc :continue (fn [buf keycode]
                         (= keycode "\""))
                        ;FIXME: Vim's <c-o> breaks history and dot repeat.
                        ;I think keep them seems a better chioce.
             :enter (fn [buf keycode]
                      (let [buf (fire-event buf :before-change-to-normal-mode)]
                        (-> buf
                            ;cancel-last-indents
                            ;update-x
                            (assoc :mode normal-mode
                                   :submode temp-normal-mode))))
             :leave (fn [buf keycode]
                      (assoc buf
                             :mode insert-mode
                             :submode 0)))))

(defn init-insert-mode-keymap [normal-mode-keymap buf]
  (let [keymap {"<esc>" nop
                "<c-o>" (fire-event
                          :temp-normal-mode-keymap
                          (temp-normal-mode-keymap normal-mode-keymap)             
                          buf)
                :else insert-mode-default 
                :continue #(not (= "<esc>" %2))
                :leave (fn [buf keycode]
                         (-> buf
                             char-
                             set-normal-mode
                             save-undo))}]
    (-> keymap
        wrap-keymap-put-insert)))

(listen :before-change-to-normal-mode
        (fn [buf]
          (-> buf
              cancel-last-indents
              (assoc :x (column buf)))))
