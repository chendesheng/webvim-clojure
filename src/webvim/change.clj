(ns webvim.change
  (:use webvim.global)
  (:import (org.ahmadsoft.ropes RopeBuilder)))

(def line-break "\n")

(defn text-new[s]
  (let [builder (RopeBuilder.)]
    (.build builder s)))

(defn text-subs
  ([s l r]
   (.subSequence s l r))
  ([s l]
   (.subSequence s l (.length s))))

(defn str-replace
  [s l r to]
  (-> s
    (.delete l r)
    (.insert l to)))

(defn apply-change[s c]
  (let [{pos :pos
         len :len
         to :to} c
        news (str-replace s pos (+ pos len) to)]
    [news {:pos pos
           :len (count to)
           :to (str (text-subs s pos (+ pos len)))}]))

(defn apply-changes[s changes]
  (reduce 
    (fn [[s rchs] c]
      (let [[news rc] (apply-change s c)]
        [news (conj rchs rc)])) [s []] changes))

(defn count-lines[s]
  (let [cnt (count line-break)]
    (loop[s1 s n 0]
      (let [i (if (empty? s1)
                -1
                (.indexOf s1 line-break))]
        (if (= i -1)
          n
          (recur (text-subs s1 (+ i cnt)) (inc n)))))))

(defn text-size
  "How many chars and lines s contains"
  [s]
  {:dpos (count s)
   :dy (count-lines s)})

(defn text-op-size
  [t op {dpos :dpos dy :dy}]
  (-> t
      (update-in [:pos] op dpos)
      (update-in [:y] op dy)))

(defn- text-apply-change[t c]
  (let [s (t :str)
        [news rc] (apply-change s c)]
    (println "text-apply-change:")
    (println c)
    (println rc)
    (-> t
        (assoc :str news)
        (update-in [:changes] conj c)
        (update-in [:pending-undo] conj rc))))

(defn- shift-pos
  ([t l from to]
   (let [s (t :str)
         pos (t :pos)
         r (+ l (count from))]
     (cond 
       (< pos l)
       t
       (>= pos r)
       (-> t
           (text-op-size - (text-size from))
           (text-op-size + (text-size to)))
       :else
       (-> t
           (text-op-size - (text-size (text-subs s l pos)))
           (text-op-size + (text-size to)))))))

(defn text-replace 
  ([t l r to]
   (let [s (t :str)
         c {:pos l
            :len (- r l)
            :to (str to)}
         newt (text-apply-change t c)
         from (-> newt :pending-undo peek :to)]
     (shift-pos newt l from to))))

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
        :else nil))))

(defn- merge-changes
  "compress changes"
  [changes]
  (reduce 
    (fn[chs c]
      (let [merged (merge-change (peek chs) c)]
        (if (nil? merged)
          (conj chs c)
          (conj (pop chs) merged)))) [] changes))

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
; --enter insert mode-- | {:changes []        :pending-undo []        undoes: []          redoes: []} 
; apply change c1       | {:changes [c1]      :pending-undo [~c1]     undoes: []          redoes: []}  
; apply change c2       | {:changes [c2]      :pending-undo [~c1 ~c2] undoes: []          redoes: []}
; --exit inert mode--   | {:changes []        :pending-undo []        undoes: [[~c1 ~c2]] redoes: []}
; undo                  | {:changes [~c2 ~c1] :pending-undo []        undoes: []          redoes: [[c2 c1]]}
; redo                  | {:changes [c1 c2]   :pending-undo []        undoes: [[~c1 ~c2]] redoes: []}
(defn text-save-undo[t]
  (let [s (t :str)
        chs (merge-changes (rseq (t :pending-undo)))]
    (println "text-save-undo:" (t :pending-undo))
    (println "text-save-undo:" chs)
    (-> t
        (update-in [:undoes] conj chs)
        (assoc :pending-undo [])
        (assoc :redoes []))))

;popup from undo apply changes (reverse order) then push reverse to redo
(defn text-undo[t]
  (if (-> t :undoes count zero?) t
    (let [s (t :str)
          chs (-> t :undoes peek rseq vec)
          [news rchs] (apply-changes s chs)]
      (-> t
          (assoc :changes chs)
          (update-in [:undoes] pop)
          (assoc :str news)
          (update-in [:redoes] conj rchs)))))

(defn text-redo[t]
  (if (-> t :redoes count zero?) t
    (let [s (t :str)
          chs (-> t :redoes peek rseq vec)
          [news rchs] (apply-changes s chs)]
      (-> t
          (assoc :changes chs)
          (update-in [:redoes] pop)
          (assoc :str news)
          (update-in [:undoes] conj rchs)))))
