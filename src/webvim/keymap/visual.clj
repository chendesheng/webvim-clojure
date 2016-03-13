(ns webvim.keymap.visual
  (:require [clojure.string :as string]
            [webvim.keymap.indent :refer [wrap-keymap-indent-visual]]
            [webvim.keymap.case :refer [wrap-keymap-case-visual]]
            [webvim.keymap.replace :refer [wrap-keymap-replace-visual]]
            [webvim.keymap.yank :refer [wrap-keymap-yank-visual]]
            [webvim.keymap.delete :refer [wrap-keymap-delete-visual]]
            [webvim.keymap.scrolling :refer [wrap-keymap-scrolling-visual]])
  (:use webvim.keymap.action
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

(defn- not-empty-range [ranges]
  (filter (fn [[a b]]
            (< a (inc b))) ranges))

(defn- clear-visual [buf]
  (-> buf
      (assoc :last-visual (-> buf :visual (dissoc :ranges))) ;keep last visual
      (assoc :visual {:type 0 :range [0 0]})))

;(:visual (set-visual-ranges (@webvim.core.ui/ui-agent :buf)))
;(make-linewise-range [82 82] (@webvim.core.ui/ui-agent :buf))

(defn- visual-select [buf]
  (let [[a b :as rg] (-> buf :context :range)]
    (if (nil? rg)
      (assoc-in buf [:visual :range 0] (buf :pos))
      (-> buf
          (assoc-in [:visual :range] [b a])
          (buf-set-pos b)))))

(defn- swap-visual-start-end [buf keycode]
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
(defn- keycode2type [keycode]
  ({"v" visual-range "V" visual-line "<c-v>" visual-block} keycode))

(defn- visual-mode-continue? [buf keycode]
  (if (-> buf :context :cancel-visual-mode?)
    false
    (let [typ (-> buf :context :last-visual-type)
          newtyp (keycode2type keycode)]
      (if (nil? newtyp)
        (not (contains? #{"A" "I" "d" "c" "y" "=" "u" "<esc>" "<" ">" "r"} keycode))
        (not (= typ newtyp))))))

(defn- change-visual-mode-type [buf keycode]
  (let [typ (-> buf :context :last-visual-type)
        newtyp (keycode2type keycode)]
    (if (= typ newtyp) buf
        (-> buf
            (assoc-in [:visual :type] newtyp)
            set-visual-ranges))))

(defn- absolute [d]
  (if (neg? d) (- d) d))

(defn- visual-line-repeat [buf append?]
  (let [keys (-> buf :context :keys pop reverse)
        dy (-> buf :context :repeat-lines :dy)
        {{lasty :lasty} :context newy :y pos :pos} buf
        lines (if (= newy lasty) ;repeat contents must not cross line
                (reverse 
                  (rest
                    (take (-> dy absolute inc) (pos-lines-seq+ (buf :str) pos))))
                '())]
    ;(println "lines:" lines)
    (reduce (fn [buf [a b]]
              (let [pos (if append? (dec b) a)]
                (-> buf
                    (buf-set-pos pos)
                    (replay-keys keys)))) 
            (assoc buf :keymap (buf :insert-mode-keymap)) lines)))

(defn- visual-line-repeat-info [buf]
  (let [[_ b] (-> buf :visual :range)
        buf1 (buf-set-pos buf b)]
    {:y (buf :y) :dy (- (buf1 :y) (buf :y))}))

(defn- repeat-ranges [{{tp :type rg :range} :visual r :str tabsize :tabsize :as buf}]
  (cond
    (= tp visual-line) (rest (map (fn [[a b]] [a (dec b)])
                                  (pos-lines-seq+ r (sort2 rg))))
    (= tp visual-block) (rest (map (fn [[a b]] [a (inc b)])
                                   (expand-block-ranges r rg tabsize)))
    :else '()))

;poses must in reverse order
(defn- repeat-insert [buf poses s]
  (reduce (fn [buf pos]
            (buf-insert buf pos s)) buf poses))

;1) set cursor pos 2) collect ranges 3) start change 4) check if it can be repeated 5) repeat changes
;repeat across ranges
(defn- start-insert-and-repeat [{{ranges :ranges} :visual :as buf} append?]
  (let [firstline (last ranges) ;ranges in reverse order
        ranges (drop-last ranges)
        poses (if append?
                (map (comp inc second) 
                     (filter (fn [[a b]]
                               (< a (inc b) (dec (pos-line-last (buf :str) a)))) ranges))
                (map first
                     (not-empty-range ranges)))
        buf (buf-set-pos buf (if append?
                               (-> firstline second inc)
                               (first firstline)))
        pos (buf :pos)]
    (-> buf
        set-insert-mode
        (assoc :keymap 
               (wrap-key (buf :insert-mode-keymap)
                         :leave (fn [handler]
                                  (fn [buf keycode]
                                    (let [newpos (buf :pos)
                                          s (if (> newpos pos)
                                              (subr (buf :str) pos newpos)
                                              nil)]
                                      (if (or (nil? s) (>= (indexr s <br>) 0))
                                        (handler buf keycode) ;only insert is allowed and it must not cross line
                                        (let [cnt (count s)
                                              poses (map #(+ % cnt) poses)]
                                          (-> buf
                                              (repeat-insert poses s)
                                              (handler keycode))))))))))))

(defn- visual-line-repeat-set-pos [{r :str :as buf} pos append?]
  (let [[[a b]] (pos-lines-seq+ r pos)
        newpos (if append? (dec b) a)]
    (buf-set-pos buf newpos)))

(defn- save-last-pos [{pos :pos y :y :as buf}]
  (-> buf
      (assoc-in [:context :lastpos] pos)
      (assoc-in [:context :lasty] y)))

(defn- visual-line-repeat-change [buf append?]
  (let [keymap (-> (buf :insert-mode-keymap)
                   (wrap-key 
                     :after (fn [handler]
                              (fn [buf keycode]
                               ;(println "I after:" keycode)
                               ;(println "repeat-lines:" (-> buf :context :repeat-lines))
                                (-> buf
                                    (handler keycode)
                                    (update-in [:context :keys] conj keycode)))))
                   (wrap-key
                     :leave (fn [handler]
                              (fn [buf keycode]
                               ;(println "repeat-lines:2" (-> buf :context :repeat-lines))
                                (-> buf
                                    (visual-line-repeat append?)
                                    (visual-line-repeat-set-pos (-> buf :context :lastpos) append?)
                                    (update-in [:context] dissoc :keys :repeat-lines :lastpos :lasty)
                                    (handler keycode))))))
        [a b] (-> buf :visual :range)]
    (-> buf
        (assoc-in [:context :repeat-lines] (visual-line-repeat-info buf))
        (visual-line-repeat-set-pos (if (< a b) a b) append?)
        save-last-pos
        set-insert-mode
        (assoc :keymap keymap))))

(defn- visual-block-lines [buf]
  (let [buf (-> buf
                set-visual-ranges)]
    (reduce (fn [items [a b]]
              (let [eol (dec (pos-line-last (buf :str) a))
                    b (min eol (inc b))]
                (conj items [(-> buf :str (subr a b)) a b]))) [] (-> buf :visual :ranges))))

;delete [a b) shift pos
(defn- shift-delete [pos a b]
  (cond
    (and (<= a pos) (< pos b)) a
    (>= pos b) (- pos (- b a))
    :else pos))

(defn- shift-ranges-delete [ranges a b]
  (if (< a b)
    (map 
      (fn [[a1 b1]]
        [(shift-delete a1 a b)
         (shift-delete b1 a b)])
      ranges) ranges))

(defn- yank-blockwise [buf items]
  (registers-put! (buf :register) {:str (string/join <br> (map first items)) :blockwise? true}))

(defn- visual-block-delete [buf]
  (let [items (visual-block-lines buf)
        buf (buf-set-pos buf (-> items last (get 1)))]
    (yank-blockwise buf (rseq items))
    (reduce (fn [buf [_ a b]]
              (-> buf
                  (update-in [:visual :ranges] shift-ranges-delete a b)
                  (buf-delete a b))) buf items)))

(defn- change-visual [linewise?]
  (start-insert-mode identity #(change-range % true linewise?)))

(defmulti visual-keymap-c (fn [buf keycode] (-> buf :visual :type)))
(defmethod visual-keymap-c visual-range [buf keycode]
  ((change-visual false) buf keycode))
(defmethod visual-keymap-c visual-line [buf keycode]
  ((change-visual true) buf keycode))
(defmethod visual-keymap-c visual-block [buf keycode]
  (-> buf
      visual-block-delete
      (start-insert-and-repeat false)))

(defmulti visual-keymap-I (fn [buf keycode] (-> buf :visual :type)))
(defmethod visual-keymap-I visual-range [buf keycode]
  (let [fnmotion (fn [buf]
                   (let [[pos _] (pos-line (buf :str) (-> buf :visual :range sort2 first))]
                     (buf-set-pos buf pos)))]
    ((start-insert-mode fnmotion identity) buf keycode)))
(defmethod visual-keymap-I visual-line [buf keycode]
  (visual-line-repeat-change buf false))
(defmethod visual-keymap-I visual-block [buf keycode]
  (start-insert-and-repeat buf false))

(defmulti visual-keymap-A (fn [buf keycode] (-> buf :visual :type)))
(defmethod visual-keymap-A visual-range [buf keycode]
  (let [[a b] (-> buf :visual :range)
        r (buf :str)
        fnmotion (if (> a b)
                   char+
                   (fn [buf]
                     (let [[pos _] (pos-line r (max a b))]
                       (buf-set-pos buf pos))))]
    ((start-insert-mode fnmotion identity) buf keycode)))
(defmethod visual-keymap-A visual-line [buf keycode]
  (visual-line-repeat-change buf true))
(defmethod visual-keymap-A visual-block [buf keycode]
  (start-insert-and-repeat buf true))

(defn- init-visual-mode-keymap [motion-keymap buf]
  (deep-merge 
    motion-keymap 
    {:enter (fn [buf keycode]
              (let [pos (buf :pos)]
                (set-visual-mode buf 
                                 {:type (keycode2type keycode)
                                  :range [pos pos]})))
     :leave (fn [buf keycode] (clear-visual buf))
     :continue visual-mode-continue?
     :before (fn [buf keycode] 
               (update-in buf [:context]
                          (fn [context]
                            (-> context
                                (assoc :last-visual-type (-> buf :visual :type)
                                       :cancel-visual-mode? false)
                                (dissoc :range)))))
     :after (fn [buf keycode]
              (-> buf
                  visual-select
                  set-visual-ranges
                  (update-x-if-not-jk keycode)))
     "o" swap-visual-start-end
     "<c-i>" nop
     "<c-o>" nop
     "<c-r>" nop
     "V" change-visual-mode-type
     "v" change-visual-mode-type
     "<c-v>" change-visual-mode-type
     "c" visual-keymap-c
     "I" visual-keymap-I
     "A" visual-keymap-A}))

(defn init-visual-mode-keymap-for-operators [motion-keymap buf]
  (let [keymap (init-visual-mode-keymap motion-keymap buf)]
    {"v" keymap
     "V" keymap
     "<c-v>" keymap}))

(defn init-visual-mode-keymap-with-operators [motion-keymap buf]
  (fire-event :visual-mode-keymap
              (-> motion-keymap 
                  (init-visual-mode-keymap buf)
                  wrap-keymap-indent-visual
                  wrap-keymap-replace-visual
                  wrap-keymap-scrolling-visual
                  wrap-keymap-yank-visual
                  wrap-keymap-delete-visual
                  wrap-keymap-case-visual) buf))

;keep track visual ranges when buffer changed
(listen
  :change-buffer
  (fn [buf _ c]
    (let [cpos (c :pos)
          delta (- (-> c :to count) (c :len))]
      (if (nil? (buf :last-visual)) buf
          (update-in buf [:last-visual :range]
                     (fn [[a b :as rg]]
                       [(if (< a cpos) a (+ a delta))
                        (if (< b cpos) b (+ b delta))]))))))
