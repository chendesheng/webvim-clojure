(ns webvim.keymap.insert
  (:use webvim.keymap.action
        webvim.core.buffer
        webvim.core.rope
        webvim.core.line
        webvim.core.pos
        webvim.core.register
        webvim.core.serve
        webvim.core.lang
        webvim.indent
        webvim.utils
        webvim.jumplist
        webvim.autocompl))

(defn- autocompl-start[t]
  (let [pos (t :pos)
        word (uncomplete-word t)
        suggestions (autocompl-suggest word)]
    ;(println "autocompl:" suggestions)
    (assoc t :autocompl 
           {:suggestions suggestions 
            :suggestions-index 0})))

(defn- autocompl-move[buf f]
  (let [b1 (if (empty? (-> buf :autocompl :suggestions))
             (autocompl-start buf)
             buf)
        i (f (-> b1 :autocompl :suggestions-index))
        cnt (-> b1 :autocompl :suggestions count)]
    (if (zero? cnt)
      b1
      (let [n (mod (+ i cnt) cnt)
            w (-> b1 :autocompl :suggestions (get n))
            s (-> b1 :autocompl :suggestions (get 0))
            ;delete back then insert word
            ks (apply conj (vec (repeat (count s) "<bs>")) (map str (vec w)))]
        (if (empty? w) buf
          (-> b1 
              (assoc-in [:autocompl :suggestions-index] n)
              (update-in [:macro :recording-keys] 
                         #(apply conj % ks)) 
              (buffer-replace-suggestion w)))))))

(defn- insert-mode-default[t keycode]
  (let [t1 (if (= "<bs>" keycode)
             (buf-delete-offset t -1)
             (buf-insert t (keycode-to-char keycode)))
        t2 (buf-update-highlight-brace-pair t1 (-> t1 :pos dec))
        t3 (if (or (indent-trigger? (t :language) keycode) (= keycode "<cr>"))
             (buf-indent-current-line t2)
             t2)]
    (if (empty? (-> t3 :autocompl :suggestions))
      t3
      (let [word (uncomplete-word t3)
            suggestions (autocompl-suggest word)]
        (if (= 1 (count suggestions))
          (assoc-in t3 [:autocompl :suggestions] [])
          (assoc t3 :autocompl 
                 (merge (:autocompl t3) 
                        {:suggestions suggestions
                         :suggestions-index 0})))))))

(defn init-insert-mode-keymap[]
  {;"<c+o>" normal-mode-keymap 
   "<c+n>" #(autocompl-move % inc)
   "<c+p>" #(autocompl-move % dec)
   "<c+r>" {"<esc>" #(assoc % :keys [])
            :else put-from-register }
   :else insert-mode-default 
   :enter set-insert-mode
   :continue #(not (= "<esc>" %2))
   :leave (fn[buf keycode]
            (-> buf
                char-backward
                update-x
                set-normal-mode
                save-undo))})

