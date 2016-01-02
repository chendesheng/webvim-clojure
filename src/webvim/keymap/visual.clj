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
        webvim.core.event
        webvim.indent
        webvim.core.utils
        webvim.jumplist
        webvim.autocompl))

(defn- set-visual-ranges[{{tp :type [a b :as rg] :range} :visual :as buf}]
  (println "set-visual-ranges:" tp rg)
  (assoc-in buf [:visual :ranges]
            (condp = tp
              visual-range (list (sort2 a b))
              visual-line (list (make-linewise-range rg buf))
              visual-block (expand-block-ranges (buf :str) a b)
              nil)))

(defn- set-visual-mode[buf, typ]
  (let [pos (buf :pos)]
    (println "set-visual-mode:" typ)
    (-> buf
        (merge {:visual {:type typ :range [pos pos]}})
        set-visual-ranges)))

(defn- clear-visual[buf]
  (-> buf
      (assoc :last-visual (buf :visual)) ;keep last visual
      (assoc :visual {:type 0 :range [0 0]})))

;(:visual (set-visual-ranges (@webvim.core.ui/ui-agent :buf)))
;(make-linewise-range [82 82] (@webvim.core.ui/ui-agent :buf))

(defn- visual-select[buf]
  (let [[a b :as rg] (-> buf :context :range)]
    (if (nil? rg)
      (assoc-in buf [:visual :range 0] (buf :pos))
      (-> buf
          (assoc-in [:visual :range] [b a])
          (buf-set-pos b)))))

(defn- swap-visual-start-end[buf]
  (let [[a b] (-> buf :visual :range)]
    (-> buf
        (assoc-in [:visual :range] [b a])
        (buf-set-pos b))))

(defn- linewise? [buf]
  (= (-> buf :visual :type) visual-line))

;type/mode    | keycode | next
;-------------|---------|-------
;normal       |  v      | visual-range
;normal       |  V      | visual-line
;visual-range |  V      | visual-line
;visual-range |  v      | normal
;visual-line  |  v      | visual-range
;visual-line  |  V      | normal
(defn- keycode2type[keycode]
  ({"v" visual-range "V" visual-line} keycode))

(defn- visual-mode-continue?[buf keycode]
  (let [typ (-> buf :context :visual-mode-type)
        newtyp (keycode2type keycode)]
    (if (nil? newtyp)
      (not (contains? #{"A" "I" "d" "c" "y" "=" "u" "<c+r>" "<esc>"} keycode))
      (not (= typ newtyp)))))

(defn- change-visual-mode-type[buf keycode]
  (let [typ (-> buf :context :visual-mode-type)
        newtyp (keycode2type keycode)]
    (if (= typ newtyp) buf
      (-> buf
        (assoc-in [:visual :type] newtyp)
        set-visual-ranges))))

(defn- change-visual[]
  (start-insert-mode "c" identity #(change-range % true (linewise? %))))

(defn- visual-line-repeat[buf first? fnmotion]
  (let [keys (-> buf :context :keys pop reverse)
        {y :y dy :dy} (-> buf :context :repeat-lines)
        {newy :y pos :pos} buf
        lines (if (= y newy) ;repeat contents must not cross line
                (cond 
                  (zero? dy) nil
                  ;repeat on each line except current line
                  (pos? dy) (reverse 
                              (rest
                                (take (inc dy) (pos-lines-seq+ (buf :str) pos))))
                  :else (rest
                          (take (- (dec dy)) (pos-lines-seq- (buf :str) pos))))
                '())]
    (fnmotion
      (reduce (fn[buf [a b]]
                (let [pos (if first? a (dec b))]
                  (-> buf
                      (buf-set-pos pos)
                      (replay-keys keys)))) 
              (assoc buf :keymap (buf :insert-mode-keymap)) lines))))

(defn- visual-line-repeat-info[buf]
  (let [[_ b] (-> buf :visual :ranges first)
        buf1 (buf-set-pos buf b)]
    {:y (buf :y) :dy (- (buf1 :y) (buf :y))}))

(defn- visual-line-repeat-change[line-first?]
  (let [fnmotion (if line-first? line-first line-end)]
    (fn[buf]
      (let [keymap (assoc (buf :insert-mode-keymap)
                     :after (fn[buf keycode]
                              (println "I after:" keycode)
                              (println "repeat-lines:" (-> buf :context :repeat-lines))
                              (let [after (or (-> buf :insert-mode-keymap :after) nop)]
                                (-> buf
                                    (after keycode)
                                    (update-in [:context :keys] conj keycode))))
                     :leave (fn[buf keycode]
                              (println "repeat-lines:2" (-> buf :context :repeat-lines))
                              (let [leave (or (-> buf :insert-mode-keymap :leave) nop)]
                                (-> buf
                                    (visual-line-repeat line-first? fnmotion)
                                    (update-in [:context] dissoc :keys)
                                    (update-in [:context] dissoc :repeat-lines)
                                    (leave keycode)))))]
                                    ;(println keymap)
        (if (= (-> buf :visual :type) visual-line)
          (let [buf (-> buf
                        fnmotion
                        (assoc-in [:context :repeat-lines] (visual-line-repeat-info buf))
                        (set-insert-mode "I") ;any keycode is ok
                        (assoc :keymap keymap))]
            (println "keymap:" (buf :keymap))
            buf) buf)))))

(defn init-visual-mode-keymap[motion-keymap pair-keymap init-type]
  (merge 
    motion-keymap 
    pair-keymap
    {"z" {"z" cursor-center-viewport}
     :enter (fn[buf keycode]
              (set-visual-mode buf init-type))
     :leave (fn[buf keycode] (clear-visual buf))
     :continue visual-mode-continue?
     :before (fn[buf keycode] 
               (-> buf
                   (assoc-in [:context :visual-mode-type]
                             (-> buf :visual :type))
                   (assoc-in [:context :range] nil)))
     :after (fn[buf keycode]
              (if (contains? #{"u" "<c+r>"} keycode)
                (update-x-if-not-jk buf keycode)
                (-> buf
                    visual-select
                    set-visual-ranges
                    (update-x-if-not-jk keycode))))
     "d" #(delete-range % true (linewise? %))
     "c" (change-visual)
     "y" #(yank-range % true (linewise? %))
     "=" #(indent-range % true)
     "o" swap-visual-start-end
     "I" (visual-line-repeat-change true)
     "A" (visual-line-repeat-change false)
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
