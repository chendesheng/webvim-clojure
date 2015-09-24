(ns webvim.change
  (:use webvim.global)
  (:import (org.ahmadsoft.ropes RopeBuilder)))

(def <br> "\n")

(defn rope[r]
  (let [builder (RopeBuilder.)]
    (.build builder r)))

(defn subr
  ([r a b]
   (.subSequence r a b))
  ([r [a b]]
   (.subSequence r a b)))

(defn replacer
  [r a b to]
  (let [r1 (if (< a b)
             (.delete r a b) r)
        r2 (if-not (empty? to)
             (.insert r1 a to) r1)]
    r2))

(defn- apply-change[r c]
  (let [{pos :pos
         len :len
         to :to} c
        newr (replacer r pos (+ pos len) to)]
    [newr {:pos pos
           :len (count to)
           :to (str (subr r pos (+ pos len)))}]))

(defn- indexr[r s]
  (if (empty? r) -1 (.indexOf r s)))

(defn count-lines[r]
  (let [cnt (count <br>)]
    (loop[r r n 0]
      (let [i (indexr r <br>)]
        (if (neg? i) n
          (recur (subr r (+ i cnt) (.length r)) (inc n)))))))

(defn rope-size
  "How many chars and lines rope contains"
  [r]
  {:dpos (.length r)
   :dy (count-lines r)})

(defn- rope-op-size
  [r op {dpos :dpos dy :dy}]
  (println r)
  (-> r
      (update-in [:pos] op dpos)
      (update-in [:y] op dy)))

;TODO: keep track of current line number is annoying
(defn buf-set-pos[buf newpos]
  (let [pos (buf :pos)
        r (buf :str)
        newpos (min newpos (-> r .length dec))]
    (cond 
      (zero? newpos)
      (-> buf 
          (assoc :y 0) 
          (assoc :pos 0))
      (> newpos pos)
      (rope-op-size buf + (rope-size (subr r pos newpos)))
      (< newpos pos)
      (rope-op-size buf - (rope-size (subr r newpos pos)))
      :else buf)))

(defn- shift-pos
  ;only need (t :pos) and (t :y)
  ([t a from to]
   (let [pos (t :pos)
         b (+ a (count from))
         szfrom (rope-size from)
         szto (rope-size to)
         t1 (cond 
              (< pos a)
              t
              (>= pos b)
              (-> t
                  (rope-op-size - szfrom)
                  (rope-op-size + szto))
              :else
              (-> t
                  (rope-op-size - (rope-size (subr from 0 (- pos a))))
                  (rope-op-size + szto)))]
     (update-in t1 [:linescnt] #(-> % (- (szfrom :dy)) (+ (szto :dy)))))))

(defn- buf-apply-change[t c]
  (let [r (t :str)
        [newr rc] (apply-change r c)]
    [(-> t
        ;keep pos after change
        (shift-pos (c :pos) (rc :to) (c :to))
        (assoc :str newr)
        (fire-event t c :change-buffer)) rc]))

(defn- buf-apply-changes[buf changes]
  (reduce 
    (fn [[buf rchs] c]
      (println c)
      (let [[newbuf rc] (buf-apply-change buf c)]
        [newbuf (conj rchs rc)])) [buf nil] changes))

(defn- push-pending[pending c oldpos]
  (if (nil? pending)
    ;create one if nil
    {:changes (list c) :cursor oldpos}
    ;don't change :cursor
    (update-in pending [:changes] conj c)))

(defn buf-replace 
  ([buf a b to]
   (if (and (= a b) (empty? to)) buf
     (let [r (buf :str)
           c {:pos a
              :len (- b a)
              :to (str to)}
           [newbuf rc] (buf-apply-change buf c)
           undo (push-pending (newbuf :pending-undo) rc (buf :pos))]
       (println "make change:" c)
       (-> newbuf
           (assoc :pending-undo undo)
           (update-in [:changes] conj c))))))

;A change is one edit at **single** point. 
;For example:
;  {:len 0 :to "aa" :pos 0}
;means insert "aa" at 0
;:pos must always be left point of changes
;
;Merge changes (changes can be merged into **one** :pos):
;  1) {:len 3 :to "" :pos 0} + {:len 0 :to "bb" :pos 0} =>  {:len 3 :to "bb" :pos 0}
;  2) {:len 0 :to "aa" :pos 0} + {:len 0 :to "bb" :pos 2} => {:len 0 :to "aabb" :pos 0}
;  3) {:len 3 :to "" :pos 0} + {:len 3 :to "" :pos 0} => {:len 6 :to "" :pos 0}
;  4) {:len 0 :to "abc" :pos 0} + {:len 1 :to "" :pos 2} => {:len 0 :to "ab" :pos 0}
;  ...
(defn- merge-change
  "return nil if can't merge (include c1 = nil) otherwise return merged change"
  [c1 c2]
  (if (nil? c1) nil
    (let [p1 (c1 :pos) p2 (c2 :pos)
          to1 (c1 :to) to2 (c2 :to)
          l1 (c1 :len) l2 (c2 :len)
          lt1 (count to1) lt2 (count to2)]
      (cond
        (= p1 p2)
        {:pos p1
         :to (str to2 (subs to1 (min lt1 l2)))
         :len (+ l1 (max 0 (- l2 lt1)))}
        (= p1 (+ p2 l2))
        {:pos p2
         :to (str to2 to1)
         :len (+ l1 l2)}
        (= (+ p1 lt1) p2)
        {:pos p1
         :to (str to1 to2)
         :len (+ l1 l2)}
        (= (+ p1 lt1) (+ p2 l2))
        {:pos (min p1 p2)
         :to (str (subs to1 0 (max (- lt1 l2) 0)) to2)
         :len (- (+ p1 l1) (min p1 p2))}
        :else nil))))

;(merge-change {:pos 2 :len 3 :to "de"} {:pos 3 :len 1 :to "aa"})
;(merge-change {:pos 2 :len 3 :to "de"} {:pos 0 :len 4 :to "aa"})
;(merge-change {:pos 2 :len 3 :to "de"} {:pos 2 :len 2 :to "aa"})

(defn- merge-changes
  "compress changes"
  [changes]
  (reduce 
    (fn[chs c]
      (println chs)
      (let [merged (merge-change (peek chs) c)]
        (if (nil? merged)
          (conj chs c)
          (conj (pop chs) merged)))) nil changes))

;(merge-changes [{:pos 8, :len 1, :to ""} 
;                {:pos 7, :len 1, :to ""} 
;                {:pos 7, :len 0, :to "a"} 
;                {:pos 8, :len 0, :to "a"} 
;                {:pos 8, :len 1, :to ""} 
;                {:pos 7, :len 1, :to ""} 
;                {:pos 6, :len 1, :to ""} 
;                {:pos 5, :len 1, :to ""} 
;                {:pos 5, :len 1, :to ""}])


;(println (merge-change {:pos 1 :len 1 :to ""} {:pos 0 :len 1 :to ""}))
;(println (merge-changes [{:pos 1 :len 1 :to ""} {:pos 0 :len 1 :to ""}]))

;test cases
;insert + insert
;(merge-change {:pos 0 :len 0 :to "aa"} {:pos 2 :len 0 :to "bb"})
;(merge-change {:pos 0 :len 0 :to "aa"} {:pos 0 :len 0 :to "bb"})

;insert + delete
;(merge-change {:pos 0 :len 0 :to "aa"} {:pos 2 :len 1 :to ""})
;(merge-change {:pos 5 :len 0 :to "aa"} {:pos 2 :len 3 :to ""})
;(merge-change {:pos 0 :len 0 :to "aa"} {:pos 0 :len 1 :to ""})
;(merge-change {:pos 0 :len 0 :to "aa"} {:pos 0 :len 3 :to ""})

;insert + replace
;(merge-change {:pos 0 :len 0 :to "aa"} {:pos 2 :len 3 :to "bb"}) ; =>  {:pos 0 :len 3 :to "aabb"}
;(merge-change {:pos 5 :len 0 :to "aa"} {:pos 2 :len 3 :to "bb"}) ; =>  {:pos 2 :len 3 :to "bbaa"}
;(merge-change {:pos 0 :len 0 :to "aa"} {:pos 0 :len 3 :to "bb"}) ; =>  {:pos 0 :len 1 :to "bb"}
;(merge-change {:pos 0 :len 0 :to "aa"} {:pos 0 :len 5 :to "bb"}) ; =>  {:pos 0 :len 3 :to "bb"}

;delete + insert
;(merge-change {:pos 0 :len 1 :to ""} {:pos 0 :len 0 :to "aa"})
;(merge-change {:pos 0 :len 3 :to ""} {:pos 0 :len 0 :to "aa"})
;(merge-change {:pos 0 :len 1 :to ""} {:pos 0 :len 0 :to "aa"})
;(merge-change {:pos 0 :len 3 :to ""} {:pos 0 :len 0 :to "aa"})
;(merge-change {:pos 0 :len 3 :to ""} {:pos 0 :len 4 :to ""})
;(merge-change {:pos 0 :len 3 :to ""} {:pos 0 :len 4 :to "bb"})
;(merge-change {:pos 0 :len 3 :to ""} {:pos 1 :len 4 :to "bb"})

; About undo/redo:
; actions               | buffer before write to client
;-----------------------|---------------------------------------------------------------------------------
; --enter insert mode-- | {:changes ()        :pending-undo ()        undoes: ()          redoes: ()} 
; apply change c1       | {:changes (c1)      :pending-undo (~c1)     undoes: ()          redoes: ()}  
; apply change c2       | {:changes (c2)      :pending-undo (~c2 ~c1) undoes: ()          redoes: ()}
; --exit inert mode--   | {:changes ()        :pending-undo ()        undoes: ((~c2 ~c1)) redoes: ()}
; undo                  | {:changes (~c2 ~c1) :pending-undo ()        undoes: ()          redoes: ((c1 c2))}
; redo                  | {:changes (c1 c2)   :pending-undo ()        undoes: ((~c2 ~c1)) redoes: ()}
(defn save-undo[buf]
  (let [pending (buf :pending-undo)]
    (if (nil? pending) buf
      (let [chs (merge-changes (pending :changes))
            undo (assoc pending :changes (reverse chs))]
        (println "text-save-undo:" pending)
        (println "text-save-undo:" chs)
        (-> buf
            (update-in [:undoes] conj undo)
            (assoc :pending-undo nil)
            (assoc :redoes nil))))))

                
;popup from undo apply changes (reverse order) then push reverse to redo
(defn- undo-redo[buf undoes redoes]
  (if (-> buf undoes count zero?) buf
    (let [undo (-> buf undoes peek)
          chs (-> undo :changes)
          [newbuf rchs] (buf-apply-changes buf chs)]
      (-> newbuf
          (assoc :changes chs)
          (buf-set-pos (undo :cursor))
          (update-in [undoes] pop)
          (update-in [redoes] 
                     conj (assoc undo :changes rchs))))))

(defn undo[buf]
  (undo-redo buf :undoes :redoes))

;(undo (active-buffer))

(defn redo[buf]
  (undo-redo buf :redoes :undoes))
