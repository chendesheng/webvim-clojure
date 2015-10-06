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
    (merge buf {:ex "" :mode visual-mode :keys nil 
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
     :continue #(not (or (= "d" %2) (= "c" %2) 
                         (= "y" %2) (= "=" %2)
                         (= "<esc>" %2) (= "v" %2)))
     :after (fn[buf keycode]
              (-> buf
                  visual-select
                  (update-x-if-not-jk (-> buf :context :lastbuf) keycode)))
     "d" #(delete-range % true)
     "c" #(change-range % true)
     "y" #(yank-range % true)
     "=" #(indent-range % true)
     "o" swap-visual-start-end}))

