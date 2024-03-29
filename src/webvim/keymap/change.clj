(ns webvim.keymap.change
  (:require
    [clojure.pprint :refer [pprint]]
    [webvim.core.event :refer [log listen]]
    [webvim.keymap.compile :refer [replay-keys wrap-key wrap-keycode]]
    [webvim.keymap.motion :refer [init-motion-keymap-for-operators]]
    [webvim.keymap.delete :refer [delete-range visual-block-delete]]
    [webvim.keymap.visual :refer [wrap-temp-visual-mode]]
    [webvim.keymap.repeat :refer [wrap-keymap-repeat-prefix]]
    [webvim.keymap.operator :refer [buf-yank set-linewise set-current-line set-current-pos
                                    make-operator set-line-end set-visual-range not-empty-range]]
    [webvim.indent :refer [buf-indent-current-line]]
    [webvim.mode :refer [set-insert-mode]]
    [webvim.core.utils :refer [sort2 deep-merge]]
    [webvim.core.rope :refer [buf-replace buf-delete buf-insert buf-set-pos subr indexr]]
    [webvim.core.pos :refer [char+ buf-start]]
    [webvim.core.line :refer [pos-line expand-block-ranges pos-lines-seq+ pos-line-last pos-line-first line-end line-start line-first trim-start]]))

(defn- start-insert-mode [f]
  (fn [buf keycode]
    (-> buf 
        f
        set-insert-mode)))

(defn- change-range [buf rg]
  (-> buf
      (delete-range rg)
      set-insert-mode))

(defn- absolute [d]
  (if (neg? d) (- d) d))

(defn- visual-line-repeat [buf append?]
  (let [keys (-> buf :context :keys pop reverse)
        dy (-> buf :context :repeat-lines :dy)
        {{lasty :lasty} :context newy :y pos :pos} buf
        lines (if (= newy lasty) ;repeat contents must not cross line
                (reverse 
                  (rest
                    (take (-> dy absolute inc) (map (partial trim-start buf)
                                                    (pos-lines-seq+ buf pos)))))
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

(defn- repeat-ranges [{{tp :type rg :range} :visual
                       tabsize :tabsize :as buf}]
  (cond
    (= tp :visual-line) (rest (map (fn [[a b]] [a b])
                                   (apply pos-lines-seq+ buf (sort2 rg))))
    (= tp :visual-block) (rest (map (fn [[a b]] [a (inc b)])
                                    (expand-block-ranges buf rg tabsize)))
    :else '()))

;poses must in reverse order
(defn- repeat-insert [buf poses r]
  (reduce (fn [buf pos]
            (buf-insert buf pos r)) buf poses))

(defn- visual-line-repeat-set-pos [buf pos append?]
  (let [[a b] (trim-start buf (pos-line buf pos))
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
                                    (update :context dissoc :keys :repeat-lines :lastpos :lasty)
                                    (handler keycode))))))
        [a b] (-> buf :visual :range)]
    (-> buf
        (assoc-in [:context :repeat-lines] (visual-line-repeat-info buf))
        (visual-line-repeat-set-pos (min a b) append?)
        save-last-pos
        set-insert-mode
        (assoc :keymap keymap))))

;1) set cursor pos 2) collect ranges 3) start change 4) check if it can be repeated 5) repeat changes
;repeat across ranges
(defn- start-insert-and-repeat [{{ranges :ranges} :visual :as buf} append?]
  (let [firstline (last ranges) ;ranges in reverse order
        ranges (drop-last ranges);visual mode range is inclusive
        poses (if append?
                (map (comp inc second) 
                     (filter (fn [[a b]]
                               (<= a b (pos-line-last buf a))) ranges))
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
                                          r (if (> newpos pos)
                                              (subr (buf :str) pos newpos))]
                                      (if (or (nil? r) (>= (indexr r "\n") 0))
                                        (handler buf keycode) ;only insert is allowed and it must not cross line
                                        (let [cnt (count r)
                                              poses (map #(+ % cnt) poses)]
                                          (-> buf
                                              (repeat-insert poses r)
                                              (handler keycode))))))))))))

(defn- visual-keymap-c [buf keycode]
  (if (-> buf :visual :type (= :visual-block))
    (-> buf
        visual-block-delete
        (start-insert-and-repeat false))
    ((make-operator set-visual-range change-range) buf keycode)))

(defn- temp-visual-keymap-c [buf keycode]
  (log (nil? buf))
  (if (-> buf :visual :type (= :visual-block))
    (-> buf
        visual-block-delete
        (start-insert-and-repeat false))
    ((make-operator change-range) buf keycode)))

(defmulti visual-keymap-I (fn [buf keycode] (-> buf :visual :type)))
(defmethod visual-keymap-I :visual-range [buf keycode]
  (let [fnmotion (fn [buf]
                   (let [[pos _] (pos-line buf (-> buf :visual :range sort2 first))]
                     (buf-set-pos buf pos)))]
    ((start-insert-mode fnmotion) buf keycode)))
(defmethod visual-keymap-I :visual-line [buf keycode]
  (visual-line-repeat-change buf false))
(defmethod visual-keymap-I :visual-block [buf keycode]
  (start-insert-and-repeat buf false))

(defmulti visual-keymap-A (fn [buf keycode] (-> buf :visual :type)))
(defmethod visual-keymap-A :visual-range [buf keycode]
  (let [[a b] (-> buf :visual :range)
        r (buf :str)
        fnmotion (if (> a b)
                   char+
                   (fn [buf]
                     (let [[pos _] (pos-line buf (max a b))]
                       (buf-set-pos buf pos))))]
    ((start-insert-mode fnmotion) buf keycode)))
(defmethod visual-keymap-A :visual-line [buf keycode]
  (visual-line-repeat-change buf true))
(defmethod visual-keymap-A :visual-block [buf keycode]
  (start-insert-and-repeat buf true))

(defn- insert-new-line [buf]
  (buf-indent-current-line
    (let [b (inc (pos-line-last buf))]
      (-> buf
          (buf-insert b "\n")
          (buf-set-pos b)))))

(defn- insert-new-line-before [buf]
  (buf-indent-current-line
    (let [a (pos-line-first buf)]
      (if (zero? a)
        (-> buf
            (buf-insert 0 "\n")
            buf-start)
        (-> buf
            (buf-set-pos (- a 1))
            (buf-insert "\n"))))))


(defn wrap-keymap-change [keymap visual-keymap]
  (let [motion-keymap (init-motion-keymap-for-operators)
        visual-keymap (wrap-temp-visual-mode visual-keymap temp-visual-keymap-c)]
    (deep-merge
      keymap
      {"i" (wrap-keycode set-insert-mode)
       "a" (start-insert-mode char+)
       "A" (start-insert-mode line-end)
       "I" (start-insert-mode line-start)
       "g" {"I" (start-insert-mode line-first)}
       "s" (make-operator set-current-pos change-range)
       "S" (make-operator set-current-line change-range)
       "o" (start-insert-mode insert-new-line)
       "O" (start-insert-mode insert-new-line-before)
       "c" (wrap-keymap-repeat-prefix
             (merge
               motion-keymap
               visual-keymap
               {"c" (make-operator set-current-line change-range)
                :after (fn [buf keycode]
                         (if (or (= keycode "c")
                                 (contains? visual-keymap keycode)
                                 (and
                                   (= (-> buf :context :lastbuf :pos) (buf :pos))
                                   (-> buf :context :range empty?)))
                           buf
                           ((make-operator change-range) buf keycode)))}))
       "C" (make-operator set-line-end change-range)})))

(listen
  :visual-mode-keymap
  (fn [keymap _]
    (assoc keymap
           "c" visual-keymap-c
           "I" visual-keymap-I
           "A" visual-keymap-A)))
