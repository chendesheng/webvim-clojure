(ns webvim.keymap.visual
  (:use webvim.keymap.action
        webvim.keymap.macro
        webvim.keymap.motion
        webvim.keymap.insert
        webvim.keymap.ex
        webvim.core.buffer
        webvim.core.rope
        webvim.core.line
        webvim.core.pos
        webvim.core.register
        webvim.core.serve
        webvim.indent
        webvim.utils
        webvim.jumplist
        webvim.autocompl))

(defn- set-visual-mode[buf]
  ;(println "set-visual-mode:")
  (let [pos (buf :pos)]
    (merge buf {:mode visual-mode :keys nil 
              :visual {:type 0 :ranges [[pos pos]]}})))

(defn- clear-visual[buf]
  (assoc buf :visual {:type 0 :ranges nil}))

(defn- visual-select[buf]
  (let [pos (buf :pos)]
    (assoc-in buf [:visual :ranges 0 0] pos)))

(defn- swap-visual-start-end[buf]
  (let [[a b] (-> buf :visual :ranges first)]
    (-> buf
        (assoc-in [:visual :ranges 0] [b a])
        (buf-set-pos b))))

(defn init-visual-mode-keymap[motion-keymap]
  (merge 
    motion-keymap 
    {"z" {"z" cursor-center-viewport}
     :enter (fn[buf keycode] 
              (set-visual-mode buf))
     :leave (fn[buf keycode] (clear-visual buf))
     :continue #(not (contains? #{"d" "c" "y" "=" "<esc>" "v"} %2))
     :after (fn[buf keycode]
              (-> buf
                  visual-select
                  (update-x-if-not-jk (-> buf :context :lastbuf) keycode)))
     "d" #(delete-range % true false)
     "c" #(change-range % true false)
     "y" #(yank-range % true false)
     "=" #(indent-range % true)
     "o" swap-visual-start-end}))

