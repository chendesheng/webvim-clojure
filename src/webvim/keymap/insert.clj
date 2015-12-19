(ns webvim.keymap.insert
  (:require [clojure.string :as string])
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

(defn- autocompl-start[buf]
  (let [pos (buf :pos)
        word (uncomplete-word buf)
        suggestions (autocompl-suggest word)]
    ;(println "autocompl:" suggestions)
    (assoc buf :autocompl 
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

(defn- insert-mode-default[buf keycode]
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
          autocompl-start
          ((fn[buf3]
            (if (-> buf3 :autocompl :suggestions count (= 1))
              (assoc buf3 :autocompl {:suggestions []
                                      :suggestions-index 0})
              buf3)))))))

(defn- normal-mode-fix-pos
    "prevent cursor on top of EOL in normal mode"
    [buf]
    (let [ch (char-at (buf :str) (buf :pos))]
      (if (= (or ch \newline) \newline)
        (char- buf) buf)))

(defn- save-dot-repeat[buf]
  (let [keyvec (-> buf :macro :recording-keys (into (buf :keys)))]
    (put-register! buf "." {:keys keyvec :str (string/join keyvec)})
    (dissoc buf :macro :recording-keys)))

(defn init-insert-mode-keymap[]
  {;"<c+o>" normal-mode-keymap 
   "<c+n>" #(autocompl-move % inc)
   "<c+p>" #(autocompl-move % dec)
   "<c+r>" {"<esc>" identity
            :else (fn[buf keycode]
                    (-> buf
                        (put-from-register keycode)
                        char+))}
   :else insert-mode-default 
   :continue #(not (= "<esc>" %2))
   :leave (fn[buf keycode]
            (-> buf
                char-
                update-x
                save-undo
                save-dot-repeat
                normal-mode-fix-pos
                set-normal-mode))})

