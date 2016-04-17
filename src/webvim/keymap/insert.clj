(ns webvim.keymap.insert
  (:require [webvim.keymap.put :refer [wrap-keymap-put-insert]]
            [webvim.mode :refer [set-normal-mode]]
            [webvim.keymap.motion :refer [re-word-start-border]])
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

(defn- delete-left-char [{pos :pos :as buf} _]
  (if (zero? pos)
    (assoc buf :beep true)
    (buf-delete buf (dec pos) pos)))

(defn- insert-keycode [{pos :pos :as buf} keycode]
  (println "keycode:" keycode)
  (buf-insert buf (keycode-to-char keycode)))

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
                (let [r (buf :str)
                      [a1 b1] (if (> (count r) a)
                                (pos-line r a))]
                  (if (and (= a1 a)
                           (<= b1 b)
                           (rblank? (subr r a1 b1)))
                    (buf-delete buf a1 (dec b1)) buf))) buf (buf :last-indents))) :last-indents))

(defn- temp-normal-mode-keymap [normal-mode-keymap]
  (-> normal-mode-keymap
      (dissoc "u" "<c-r>") ;can't make undo/redo in the middle of change
      (assoc :continue (fn [buf keycode]
                         (= keycode "\"")) ;FIXME: repeat count prefix???
                        ;FIXME: Vim's <c-o> breaks history and dot repeat.
                        ;I think keep them seems a better chioce.
             :enter (fn [buf keycode]
                      (let [buf (fire-event buf :before-change-to-normal-mode)]
                        (-> buf
                            ;cancel-last-indents
                            ;update-x
                            (assoc :mode :normal-mode
                                   :submode :temp-normal-mode))))
             :leave (fn [buf keycode]
                      (assoc buf
                             :mode :insert-mode
                             :submode :none)))))

(defn init-insert-mode-keymap [normal-mode-keymap buf]
  ;TODO: <c-u> <c-w>
  (let [keymap {"<esc>" nop
                "<c-o>" (fire-event
                          :temp-normal-mode-keymap
                          (temp-normal-mode-keymap normal-mode-keymap)
                          buf)
                "<c-u>" (fn [buf keycode]
                          (let [r (buf :str)
                                pos (buf :pos)
                                newpos (pos-line-start r pos)]
                            (if (<= pos newpos)
                              (assoc buf :beep true)
                              (buf-delete buf newpos pos))))
                "<bs>" delete-left-char
                "<c-w>" (fn [{pos :pos :as buf} keycode]
                          (if (zero? pos)
                            (assoc buf :beep true)
                            (let [r (buf :str)
                                  lang (buf :language)
                                  newpos (or (first (pos-re- r (dec pos) (re-word-start-border lang))) 0)]
                              (buf-delete buf newpos pos))))
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
