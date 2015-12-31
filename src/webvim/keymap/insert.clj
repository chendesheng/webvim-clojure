(ns webvim.keymap.insert
  (:use webvim.keymap.action
        webvim.core.buffer
        webvim.core.rope
        webvim.core.line
        webvim.core.pos
        webvim.core.register
        webvim.core.lang
        webvim.indent
        webvim.core.utils
        webvim.jumplist
        webvim.autocompl))

(defn- new-autocompl[buf]
  (if (-> buf :autocompl nil?) 
    (let [w (uncomplete-word buf)]
      (if (nil? w) buf
        (assoc buf :autocompl
               ;remove current word
               ;words is fixed during auto complete
               {:words (keys (autocompl-remove-word @autocompl-words w))
                :suggestions nil
                :index 0
                :uncomplete-word uncomplete-word
                :replace buffer-replace-suggestion
                :limit-number 0})))
    buf))

(defn- filter-not-in-range[indents pos]
  (drop-while (fn[[a b]]
                (>= b pos)) indents))

(defn- insert-keycode[{pos :pos :as buf} keycode]
  (if (= "<bs>" keycode)
    (if (zero? pos) buf (buf-delete buf (dec pos) pos))
    (buf-insert buf (keycode-to-char keycode))))

(defn- insert-mode-default[buf keycode]
  (println "insert-mode-default: " keycode)
  (let [pos (buf :pos)
        buf1 (insert-keycode buf keycode)
        buf2 (buf-update-highlight-brace-pair buf1 (-> buf1 :pos dec))
        buf3 (if (or (indent-trigger? (buf :language) keycode) (= keycode "<cr>"))
               (buf-indent-current-line buf2)
               buf2)]
    ;continue checking until there is no suggestions
    (if (-> buf3 :autocompl nil?)
      buf3
      (autocompl-suggest buf3))))

(defn- cancel-last-indents[buf]
  (dissoc (if (-> buf :last-indents empty?)
            buf
            (reduce
              (fn[buf [a b]]
                ;(println "cancel-last-indent:" a b)
                (if (= (char-at (buf :str) (dec b)) \newline)
                  (buf-delete buf a (dec b)) buf)) buf (buf :last-indents))) :last-indents))

(defn init-insert-mode-keymap[]
  {;"<c+o>" normal-mode-keymap 
   "<c+n>" #(autocompl-move (new-autocompl %) inc)
   "<c+p>" #(autocompl-move (new-autocompl %) dec)
   "<c+r>" {"<esc>" identity
            :else (fn[buf keycode]
                    (-> buf
                        (put-from-register keycode)
                        char+))}
   "<esc>" identity
   :after (fn[buf keycode]
            (println "insert after:" keycode) buf)
   :else insert-mode-default 
   :continue #(not (= "<esc>" %2))
   :leave (fn[buf keycode]
            (-> buf
                (dissoc :autocompl)
                cancel-last-indents
                char-
                update-x
                save-dot-repeat
                save-undo
                normal-mode-fix-pos
                set-normal-mode))})

