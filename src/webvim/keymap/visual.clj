(ns webvim.keymap.visual
  (:require [clojure.string :as string])
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

(defn- set-visual-ranges[{{tp :type rg :range} :visual :as buf}]
  (println "set-visual-ranges:" tp rg)
  (assoc-in buf [:visual :ranges]
            (condp = tp
              visual-range (list (sort2 rg))
              visual-line (list (make-linewise-range rg buf))
              visual-block (into '() (expand-block-ranges (buf :str) rg (buf :tabsize)))
              nil)))

(defn- set-visual-mode[buf, typ]
  (let [pos (buf :pos)]
    (println "set-visual-mode:" typ)
    (-> buf
        (merge {:visual {:type typ :range [pos pos]}})
        set-visual-ranges)))

(defn- clear-visual[buf]
  (-> buf
      (assoc :last-visual (-> buf :visual (dissoc :ranges))) ;keep last visual
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

;type/mode    | keycode | next
;-------------|---------|-------
;normal       |  v      | visual-range
;normal       |  V      | visual-line
;visual-range |  V      | visual-line
;visual-range |  v      | normal
;visual-line  |  v      | visual-range
;visual-line  |  V      | normal
(defn- keycode2type[keycode]
  ({"v" visual-range "V" visual-line "<c+v>" visual-block} keycode))

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

(defn- change-visual[linewise?]
  (start-insert-mode "c" identity #(change-range % true linewise?)))

(defn- absolute[d]
  (if (neg? d) (- d) d))

(defn- visual-line-repeat[buf append?]
  (let [keys (-> buf :context :keys pop reverse)
        dy (-> buf :context :repeat-lines :dy)
        {{lasty :lasty} :context newy :y pos :pos} buf
        lines (if (= newy lasty) ;repeat contents must not cross line
                (reverse 
                  (rest
                    (take (-> dy absolute inc) (pos-lines-seq+ (buf :str) pos))))
                '())]
    ;(println "lines:" lines)
    (reduce (fn[buf [a b]]
              (let [pos (if append? (dec b) a)]
                (-> buf
                    (buf-set-pos pos)
                    (replay-keys keys)))) 
            (assoc buf :keymap (buf :insert-mode-keymap)) lines)))

(defn- visual-line-repeat-info[buf]
  (let [[_ b] (-> buf :visual :range)
        buf1 (buf-set-pos buf b)]
    {:y (buf :y) :dy (- (buf1 :y) (buf :y))}))

(defn- repeat-ranges[{{tp :type rg :range} :visual r :str tabsize :tabsize :as buf}]
  (cond
    (= tp visual-line) (rest (map (fn[[a b]] [a (dec b)])
                                  (pos-lines-seq+ r (sort2 rg))))
    (= tp visual-block) (rest (map (fn[[a b]] [a (inc b)])
                                   (expand-block-ranges r rg tabsize)))
    :else '()))

;poses must in reverse order
(defn- repeat-insert[buf poses s]
  (reduce (fn[buf pos]
            (buf-insert buf pos s)) buf poses))

;1) set cursor pos 2) collect ranges 3) start change 4) check if it can be repeated 5) repeat changes
;repeat across ranges
(defn- start-insert-and-repeat[{{ranges :ranges} :visual :as buf} append?]
  (let [firstline (last ranges) ;ranges in reverse order
        ranges (drop-last ranges)
        poses (if append?
                (map (comp inc second) 
                     (filter (fn[[a b]]
                               (< a (inc b) (dec (pos-line-last (buf :str) a)))) ranges))
                (map first
                     (filter (fn[[a b]]
                               (< a (inc b))) ranges)))
        buf (buf-set-pos buf (if append?
                               (-> firstline second inc)
                               (first firstline)))
        pos (buf :pos)]
    (-> buf
        set-insert-mode
        (assoc :keymap 
               (assoc (buf :insert-mode-keymap)
                      :leave (fn[buf keycode]
                               (let [leave (or (-> buf :insert-mode-keymap :leave) nop)
                                     newpos (buf :pos)
                                     s (if (> newpos pos)
                                         (subr (buf :str) pos newpos)
                                         nil)]
                                 (if (or (nil? s) (>= (indexr s <br>) 0))
                                   (leave buf keycode) ;only insert is allowed and it must not cross line
                                   (let [cnt (count s)
                                         poses (map #(+ % cnt) poses)]
                                     (-> buf
                                         (repeat-insert poses s)
                                         (leave keycode)))))))))))

(defn- visual-line-repeat-set-pos[{r :str :as buf} pos append?]
  (let [[[a b]] (pos-lines-seq+ r pos)
        newpos (if append? (dec b) a)]
    (buf-set-pos buf newpos)))

(defn- save-last-pos[{pos :pos y :y :as buf}]
  (-> buf
      (assoc-in [:context :lastpos] pos)
      (assoc-in [:context :lasty] y)))

(defn- visual-line-repeat-change[append?]
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
                                       (visual-line-repeat append?)
                                       (visual-line-repeat-set-pos (-> buf :context :lastpos) append?)
                                       (update-in [:context] dissoc :keys :repeat-lines :lastpos)
                                       (leave keycode)))))
          [a b] (-> buf :visual :range)]
      (-> buf
          (assoc-in [:context :repeat-lines] (visual-line-repeat-info buf))
          (visual-line-repeat-set-pos (if (< a b) a b) append?)
          save-last-pos
            ;(assoc-in [:context :lastpos] (buf :pos))
          set-insert-mode
          (assoc :keymap keymap)))))

(defn- visual-block-reduce[buf fn]
  (let [buf (-> buf
                (buf-set-pos (apply min (-> buf :visual :range)))
                set-visual-ranges)]
    (println "ranges:" (-> buf :visual :ranges))
    (reduce fn buf (-> buf :visual :ranges))))

;delete [a b) shift pos
(defn- shift-delete[pos a b]
  (cond
    (and (<= a pos) (< pos b)) a
    (>= pos b) (- pos (- b a))
    :else pos))

(defn- shift-ranges-delete[ranges a b]
  (if (< a b)
    (map 
      (fn[[a1 b1]]
        [(shift-delete a1 a b)
         (shift-delete b1 a b)])
      ranges) ranges))

;TODO: yank
(defn- visual-block-delete[buf]
  (let [buf (visual-block-reduce 
              buf (fn[buf [a b]]
                    (let [eol (dec (pos-line-last (buf :str) a))
                          b (min eol (inc b))]
                      (-> buf
                          (update-in [:visual :ranges] shift-ranges-delete a b)
                          (update-in [:context :tmp] conj (-> buf :str (subr a b)))
                          (buf-delete a b)))))]
    (put-register! buf "\"" {:str (string/join <br> (-> buf :context :tmp )) :blockwise? true})
    (update-in buf [:context] dissoc :tmp)))

(defn- visual-mode-keymap[motion-keymap pair-keymap]
  (merge 
    motion-keymap 
    pair-keymap
    {"z" {"z" cursor-center-viewport}
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
     "=" #(indent-range % true)
     "o" swap-visual-start-end
     "u" undo
     "<c+r>" redo
     "V" #(change-visual-mode-type % "V")
     "v" #(change-visual-mode-type % "v")
     "<c+v>" #(change-visual-mode-type % "<c+v>")}))

(defn init-visual-range-keymap[motion-keymap pair-keymap]
  (merge
    (visual-mode-keymap motion-keymap pair-keymap)
    {:enter (fn[buf keycode]
              (set-visual-mode buf visual-range))
     "d" #(delete-range % true false)
     "c" (change-visual false)
     "y" #(yank-range % true false)}))

(defn init-visual-line-keymap[motion-keymap pair-keymap]
  (merge
    (visual-mode-keymap motion-keymap pair-keymap)
    {:enter (fn[buf keycode]
              (set-visual-mode buf visual-line))
     "I" (visual-line-repeat-change false)
     "A" (visual-line-repeat-change true)
     "d" #(delete-range % true true)
     "c" (change-visual true)
     "y" #(yank-range % true true)}))

(defn init-visual-block-keymap[motion-keymap pair-keymap]
  (merge
    (visual-mode-keymap motion-keymap pair-keymap)
    {:enter (fn[buf keycode]
              (set-visual-mode buf visual-block))
     "d" visual-block-delete
     "c" (fn[buf]
           (-> buf
               visual-block-delete
               (start-insert-and-repeat false)))
     "I" (fn[buf]
           (start-insert-and-repeat buf false))
     "A" (fn[buf]
           (start-insert-and-repeat buf true))}))

;keep track visual ranges when buffer changed
(defonce ^:private listen-change-buffer 
  (listen
    :change-buffer
    (fn [buf _ c]
      (let [cpos (c :pos)
            delta (- (-> c :to count) (c :len))]
        (if (nil? (buf :last-visual)) buf
          (update-in buf [:last-visual :range]
                     (fn[[a b :as rg]]
                       [(if (< a cpos) a (+ a delta))
                        (if (< b cpos) b (+ b delta))])))))))
