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
        webvim.core.event
        webvim.indent
        webvim.utils
        webvim.jumplist
        webvim.autocompl))

(defn- set-visual-mode[buf, typ]
  ;(println "set-visual-mode:")
  (let [pos (buf :pos)]
    (merge buf {:mode visual-mode :keys nil 
              :visual {:type typ :ranges [[pos pos]]}})))

(defn- clear-visual[buf]
  (-> buf
      (assoc :last-visual (buf :visual)) ;keep last visual
      (assoc :visual {:type 0 :ranges nil})))

(defn- visual-select[buf]
  (let [pos (buf :pos)]
    (assoc-in buf [:visual :ranges 0 0] pos)))

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
(defn- visual-mode-continue?[buf keycode]
  (let [typ (-> buf :context :visual-mode-type)]
    (cond
      (and (= typ visual-normal) (= keycode "V"))
      true
      (and (= typ visual-line) (= keycode "v"))
      true
      (and (= typ visual-normal) (= keycode "v"))
      false
      (and (= typ visual-line) (= keycode "V"))
      false
      :else (not (contains? #{"d" "c" "y" "=" "<esc>"} keycode)))))

(defn init-visual-mode-keymap[motion-keymap current-type]
  (merge 
    motion-keymap 
    {"z" {"z" cursor-center-viewport}
     :enter (fn[buf keycode]
              (set-visual-mode buf current-type))
     :leave (fn[buf keycode] (clear-visual buf))
     :continue visual-mode-continue?
     :before (fn[buf keycode] (assoc-in buf [:context :visual-mode-type] (-> buf :visual :type)))
     :after (fn[buf keycode]
              (-> buf
                  visual-select
                  (update-x-if-not-jk (-> buf :context :lastbuf) keycode)))
     "d" #(delete-range % true (linewise? %))
     "c" #(change-range % true (linewise? %))
     "y" #(yank-range % true (linewise? %))
     "=" #(indent-range % true)
     "o" swap-visual-start-end
     "V" (fn[buf]
            (let [typ (-> buf :visual :type)]
              (cond 
                (= typ visual-line)
                (assoc-in buf [:visual :type] visual-normal)
                (= typ visual-normal)
                (assoc-in buf [:visual :type] visual-line)
                :else buf)))
     "v" (fn[buf]
            (let [typ (-> buf :visual :type)]
              (if (= typ visual-normal) buf
                (assoc-in buf [:visual :type] visual-normal))))}))

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
