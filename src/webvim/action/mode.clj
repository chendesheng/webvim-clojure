(ns webvim.action.mode
  (:use webvim.action.edit
        webvim.core.pos
        webvim.core.line
        webvim.core.serve
        webvim.indent
        webvim.register))

(defonce motion-keymap (atom {}))
(defonce edit-keymap (atom {}))
(defonce normal-mode-keymap (atom {}))
(defonce visual-mode-keymap (atom {}))
(defonce insert-mode-keymap (atom {}))
(defonce ex-mode-keymap (atom {}))

(defonce normal-mode 0)
(defonce insert-mode 1)
(defonce visual-mode 2)
(defonce ex-mode 3)

(defn set-normal-mode[b]
  ;(println "set-normal-mode:")
  (merge b {:ex "" 
            :mode normal-mode 
            :keys nil
            :visual {:type 0 :ranges nil}
            :autocompl {:suggestions nil 
                        :suggestions-index 0}}))

(defn set-visual-mode[b]
  ;(println "set-visual-mode:")
  (let [pos (b :pos)]
    (merge b {:ex "" :mode visual-mode :keys nil 
              :visual {:type 0 :ranges [[pos pos]]}})))

(defn set-insert-mode[b keycode]
  ;(println "set-insert-mode")
  (-> b 
      (assoc-in [:visual :ranges] nil)
      (merge {:ex "" :mode insert-mode :message nil :keys nil})))

(defn set-insert-append[b keycode]
    (-> b
        char-forward
        (set-insert-mode keycode)))

(defn set-insert-line-end[b keycode]
  (-> b
      line-end
      (set-insert-mode keycode)))

(defn set-insert-line-start[b keycode]
  (-> b
      line-start
      (set-insert-mode keycode)))

(defn set-insert-remove-char[b keycode]
  (let [pos (b :pos)]
    (registers-put (b :registers) (-> b :context :register) (buf-copy-range b pos pos true))
    (-> b
        (set-insert-mode keycode)
        (buf-delete-offset 1))))

(defn change-range[buf]
  (let [[a b] (-> buf :visual :ranges first)]
    (-> buf
        (delete-inclusive a b)
        (set-insert-mode "c")
        (serve-keymap (@normal-mode-keymap "i") "c"))))

(defn change-to-line-end[buf]
  (-> buf
      (buf-delete (buf :pos) (-> buf current-line last dec))
      (serve-keymap (@normal-mode-keymap "i") "c")))

(defn set-insert-new-line[buf keycode]
  (-> buf 
      (set-insert-mode keycode)
      insert-line-after
      buf-indent-current-line))

(defn set-insert-new-line-before[buf keycode]
  (-> buf 
      (set-insert-mode keycode)
      insert-line-before
      buf-indent-current-line))

