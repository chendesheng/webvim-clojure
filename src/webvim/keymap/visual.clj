(ns webvim.keymap.visual
  (:use webvim.keymap.action
        webvim.keymap.motion
        webvim.keymap.insert
        webvim.keymap.ex
        webvim.core.buffer
        webvim.core.rope
        webvim.core.line
        webvim.core.pos
        webvim.core.register
        webvim.core.serve
        webvim.core.event
        webvim.indent
        webvim.core.utils
        webvim.jumplist
        webvim.autocompl))

(defn- set-visual-mode[buf, typ]
  ;(println "set-visual-mode:")
  (let [pos (buf :pos)]
    (merge buf {:mode visual-mode
                :visual {:type typ :ranges [[pos pos]]}})))

(defn- clear-visual[buf]
  (-> buf
      (assoc :last-visual (buf :visual)) ;keep last visual
      (assoc :visual {:type 0 :ranges nil})))

(defn- visual-select[buf]
  (let [[a b :as rg] (-> buf :context :range)]
    (if (nil? rg)
      (assoc-in buf [:visual :ranges 0 0] (buf :pos))
      (-> buf
          (assoc-in [:visual :ranges 0] [b a])
          (buf-set-pos b)))))

(defn- swap-visual-start-end[buf]
  (let [[a b] (-> buf :visual :ranges first)]
    (-> buf
        (assoc-in [:visual :ranges 0] [b a])
        (buf-set-pos b))))

(defn- linewise? [buf]
  (= (-> buf :visual :type) visual-line))

;type/mode    | keycode | next
;-------------|---------|-------
;normal       |  v      | visual-normal
;normal       |  V      | visual-line
;visual-normal|  V      | visual-line
;visual-normal|  v      | normal
;visual-line  |  v      | visual-normal
;visual-line  |  V      | normal
(defn- keycode2type[keycode]
  ({"v" visual-normal "V" visual-line} keycode))

(defn- visual-mode-continue?[buf keycode]
  (let [typ (-> buf :context :visual-mode-type)
        newtyp (keycode2type keycode)]
    (if (nil? newtyp)
      (not (contains? #{"d" "c" "y" "=" "u" "<c+r>" "<esc>"} keycode))
      (not (= typ newtyp)))))

(defn- change-visual-mode-type[buf keycode]
  (let [typ (-> buf :context :visual-mode-type)
        newtyp (keycode2type keycode)]
    (if (= typ newtyp) buf
      (assoc-in buf [:visual :type] newtyp))))

(defn init-visual-mode-keymap[motion-keymap pair-keymap current-type]
  (merge 
    motion-keymap 
    pair-keymap
    {"z" {"z" cursor-center-viewport}
     :enter (fn[buf keycode]
              (set-visual-mode buf current-type))
     :leave (fn[buf keycode] (clear-visual buf))
     :continue visual-mode-continue?
     :before (fn[buf keycode] 
               (-> buf
                   (assoc-in [:context :visual-mode-type]
                             (-> buf :visual :type))
                   (assoc-in [:context :range] nil)))
     :after (fn[buf keycode]
              (if (contains? #{"u" "<c+r"} keycode)
                (update-x-if-not-jk buf keycode)
                (-> buf
                    visual-select
                    (update-x-if-not-jk keycode))))
     "d" #(delete-range % true (linewise? %))
     "c" #(change-range % true (linewise? %))
     "y" #(yank-range % true (linewise? %))
     "=" #(indent-range % true)
     "o" swap-visual-start-end
     "u" undo
     "<c+r>" redo
     "V" #(change-visual-mode-type % "V")
     "v" #(change-visual-mode-type % "v")}))

;keep track visual ranges when buffer changed
(defonce ^:private listen-change-buffer 
  (listen
    :change-buffer
    (fn [buf oldbuf c]
      (let [bufid (buf :id)
            cpos (c :pos)
            delta (- (-> c :to count) (c :len))]
        (update-in buf [:last-visual :ranges]
                   (fn [ranges]
                     (map (fn[[a b :as rg]]
                            [(if (< a cpos) a (+ a delta))
                             (if (< b cpos) b (+ b delta))])
                          ranges)))))))
