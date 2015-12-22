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
  (if (-> buf :autocompl :suggestions nil?) 
    (assoc buf :autocompl
           {:words (keys @autocompl-words)
            :suggestions nil
            :suggestions-index 0
            :uncomplete-word uncomplete-word
            :replace buffer-replace-suggestion
            :limit-number 0})
    buf))

(defn- insert-mode-default[buf keycode]
  (println "insert-mode-default: " keycode)
  (let [pos (buf :pos)
        buf1 (if (= "<bs>" keycode)
               (if (zero? pos) buf (buf-delete buf (dec pos) pos))
               (buf-insert buf (keycode-to-char keycode)))
        buf2 (buf-update-highlight-brace-pair buf1 (-> buf1 :pos dec))
        buf3 (if (or (indent-trigger? (buf :language) keycode) (= keycode "<cr>"))
               (buf-indent-current-line buf2)
               buf2)]
    (if (empty? (-> buf3 :autocompl :suggestions))
      buf3
      (-> buf3
          autocompl-suggest
          ((fn[buf3]
            (if (-> buf3 :autocompl :suggestions count (= 1))
              (update-in buf3 [:autocompl] merge {:suggestions []
                                                  :suggestions-index 0})
              buf3)))))))

(defn init-insert-mode-keymap[]
  {;"<c+o>" normal-mode-keymap 
   "<c+n>" #(autocompl-move (new-autocompl %) inc)
   "<c+p>" #(autocompl-move (new-autocompl %) dec)
   "<c+r>" {"<esc>" identity
            :else (fn[buf keycode]
                    (-> buf
                        (put-from-register keycode)
                        char+))}
   :after (fn[buf keycode]
            (println "insert after:" keycode) buf)
   :else insert-mode-default 
   :continue #(not (= "<esc>" %2))
   :leave (fn[buf keycode]
            (-> buf
                (dissoc buf :autocompl)
                char-
                update-x
                save-dot-repeat
                save-undo
                normal-mode-fix-pos
                set-normal-mode))})

