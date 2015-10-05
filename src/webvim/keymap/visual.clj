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

(defn- visual-mode-select[buf keycode]
  (let [m (re-find #"[ocdy]" keycode)] ;don't change cursor position if not motion
    (if (nil? m)
      (let [pos (buf :pos)]
        (update-in buf [:visual :ranges 0] 
                   (fn[[a b]] [pos b]))) buf)))

(defn- swap-visual-start-end[buf]
  (let [[a b] (-> buf :visual :ranges first)
        newbuf (-> buf
                 (assoc-in [:visual :ranges 0] [b a])
                 (buf-set-pos b))]
    (assoc-in newbuf [:context :lastbuf] newbuf)))

(defn init-visual-mode-keymap[motion-keymap]
  (merge 
    motion-keymap 
    {"z" {"z" cursor-center-viewport}
     "y" #(yank-range % true)
     :enter (fn[buf keycode] 
              (-> buf
                  set-visual-mode
                  (assoc-in [:context :lastbuf] buf)))
     :leave (fn[buf keycode] (set-normal-mode buf))
     :continue #(not (or (= "d" %2) (= "c" %2) (= "<esc>" %2) (= "v" %2) (= "y" %2)))
     :after (fn[buf keycode]
              (-> buf
                  (visual-mode-select keycode)
                  (update-x-if-not-jk (buf :lastbuf) keycode)))
     "d" #(delete-range % true)
     "c" #(change-range % true)
     "=" #(indent-range % true)
     "o" swap-visual-start-end}))

