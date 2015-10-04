(ns webvim.action.mode
  (:use webvim.core.pos
        webvim.core.line
        webvim.core.serve
        webvim.core.register
        webvim.action.edit
        webvim.indent))

(defonce normal-mode 0)
(defonce insert-mode 1)
(defonce visual-mode 2)
(defonce ex-mode 3)

(defn set-normal-mode[buf]
  ;(println "set-normal-mode:")
  (merge buf {:ex "" 
            :mode normal-mode 
            :keys nil
            :visual {:type 0 :ranges nil}
            :autocompl {:suggestions nil 
                        :suggestions-index 0}}))

(defn set-visual-mode[buf]
  ;(println "set-visual-mode:")
  (let [pos (buf :pos)]
    (merge buf {:ex "" :mode visual-mode :keys nil 
              :visual {:type 0 :ranges [[pos pos]]}})))

(defn set-insert-mode[buf keycode]
  ;(println "set-insert-mode")
  (-> buf 
      (assoc-in [:visual :ranges] nil)
      (merge {:ex "" :mode insert-mode :message nil :keys nil})))

(defn set-insert-append[buf keycode]
    (-> buf
        char-forward
        (set-insert-mode keycode)))

(defn set-insert-line-end[buf keycode]
  (-> buf
      line-end
      (set-insert-mode keycode)))

(defn set-insert-line-start[buf keycode]
  (-> buf
      line-start
      (set-insert-mode keycode)))

(defn set-insert-remove-char[buf keycode]
  (let [pos (buf :pos)]
    (registers-put (buf :registers) (-> buf :context :register) (buf-copy-range buf pos pos true))
    (-> buf
        (set-insert-mode keycode)
        (buf-delete-offset 1))))

(defn change-range[buf]
  (let [[a b] (-> buf :visual :ranges first)]
    (-> buf
        (delete-inclusive a b)
        (set-insert-mode "c")
        (serve-keymap (-> buf :root-keymap (get "i")) "c"))))

(defn change-to-line-end[buf]
  (-> buf
      (buf-delete (buf :pos) (-> buf current-line last dec))
      (serve-keymap (-> buf :root-keymap (get "i")) "c")))

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

