(ns webvim.keymap.insert
  (:use webvim.keymap.action
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
        buf1 (insert-keycode buf keycode)
        buf2 (buf-match-bracket buf1 (-> buf1 :pos dec))]
    (if (or (indent-trigger? (buf :language) keycode) (= keycode "<cr>"))
      (buf-indent-current-line buf2)
      buf2)))

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
                      (-> buf
                          cancel-last-indents
                          update-x
                          normal-mode-fix-pos
                          (assoc :mode normal-mode
                                 :submode temp-normal-mode)))
             :leave (fn [buf keycode]
                      (assoc buf
                             :mode insert-mode
                             :submode 0)))))

(defn init-insert-mode-keymap [normal-mode-keymap buf]
  {"<c-r>" {"<esc>" nop
            :else (fn [buf keycode]
                    (-> buf
                        (put-from-register keycode false)
                        char+))}
   "<esc>" nop
   "<c-o>" (fire-event
             :temp-normal-mode-keymap
             (temp-normal-mode-keymap normal-mode-keymap)             
             buf)
   :else insert-mode-default 
   :continue #(not (= "<esc>" %2))
   :leave (fn [buf keycode]
            (-> buf
                cancel-last-indents
                char-
                update-x
                save-undo
                normal-mode-fix-pos
                set-normal-mode))})

