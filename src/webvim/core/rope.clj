(ns webvim.core.rope
  (:require [webvim.core.lineindex :refer [update-lineindex pos-linenum]])
  (:use webvim.core.event
        webvim.core.utils
        webvim.core.parallel-universe)
  (:import (org.ahmadsoft.ropes RopeBuilder)))

(def <br> "\n")

(defn rope [r]
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

(defn- apply-change [r c]
  (let [{pos :pos
         len :len
         to :to} c
        newr (replacer r pos (+ pos len) to)]
    [newr {:pos pos
           :len (count to)
           :to (str (subr r pos (+ pos len)))}]))

(defn indexr [r s]
  (if (empty? r) -1 (.indexOf r s)))

(defn rblank? 
  ([s]
    (or (nil? s)
        (let [m (.matcher s #"^\s*$")]
          (.find m 0))))
  ([s rg]
    (rblank? (subr s rg))))

(defn count-<br> [r]
  (let [cnt (count <br>)]
    (loop [r r n 0]
      (let [i (indexr r <br>)]
        (if (neg? i) n
            (recur (subr r (+ i cnt) (.length r)) (inc n)))))))

(defn rope-size
  "How many chars and lines rope contains"
  [r]
  {:dpos (.length r)
   :dy (count-<br> r)})

(defn- rope-op-size
  [r op {dpos :dpos dy :dy}]
  ;(println r)
  (-> r
      (update :pos op dpos)
      (update :y op dy)))

(defn- fix-utf16-pos [r pos]
  (let [ch (.charAt r pos)]
    (if (or (variation-selector? ch)
            (and (surrogate? ch)
                 (even? (count (take-while surrogate? (iterator-seq (.reverseIterator r (- (count r) pos 1))))))))
      (dec pos)
      pos)))

;TODO: keep track of current line number is annoying
(defn buf-set-pos [buf newpos]
  (let [r (buf :str)
        pos (buf :pos)
        newpos (fix-utf16-pos r (min newpos (-> r .length dec)))]
    (cond 
      (zero? newpos)
      (assoc buf :y 0 :pos 0)
      (= pos newpos)
      buf
      :else
      (assoc buf
             :y (-> buf :lineindex (pos-linenum newpos))
             :pos newpos))))

(defn- shift-pos
  ;only need (buf :pos) and (buf :y)
  [buf a from to]
  (let [pos (buf :pos)
        b (+ a (count from))
        szfrom (rope-size from)
        szto (rope-size to)
        t1 (cond 
             (< pos a)
             buf
             (>= pos b)
             (-> buf
                 (rope-op-size - szfrom)
                 (rope-op-size + szto))
             :else
             (-> buf
                 (rope-op-size - (rope-size (subr from 0 (- pos a))))
                 (rope-op-size + szto)))]
    (update t1 :linescnt #(-> % (- (szfrom :dy)) (+ (szto :dy))))))

(defn- buf-apply-change [buf c]
  (let [r (buf :str)
        [newr rc] (apply-change r c)]
    [(-> buf
         ;keep pos after change
         (shift-pos (c :pos) (rc :to) (c :to))
         (assoc :str newr)
         (update :lineindex update-lineindex c)
         (fire-event buf c :change-buffer)) rc]))

(defn- buf-apply-changes [buf changes]
  (reduce 
    (fn [[buf rchs] c]
      ;(println c)
      (let [[newbuf rc] (buf-apply-change buf c)]
        [newbuf (conj rchs rc)])) [buf nil] changes))

(defn- push-pending [pending c oldpos]
  (if (nil? pending)
    ;create one if nil
    {:changes (list c) :cursor oldpos}
    ;don't change :cursor
    (update pending :changes conj c)))

(defn last-tabstop [r]
  (loop [i 0 it (.reverseIterator r)] 
    (if (.hasNext it)
      (let [ch (.next it)]
        (cond 
          (= ch \tab) i
          (= ch \newline) i
          :else (recur (inc i) it)))
      i)))

(defn expand-tab [s idx tabsize]
  (loop [i 0 ret ""]
    (let [nexti (.indexOf s "\t" i)]
      ;(println nexti)
      (if (neg? nexti)
        (str ret (subs s i))
        (recur (inc nexti) 
               (let [ret (str ret (subs s i nexti))]
                 (str ret 
                      (repeat-chars 
                        (- tabsize (rem (+ (.length ret) idx) tabsize)) \space))))))))

(defn buf-replace 
  ([buf a b to]
    (if (and (= a b) (empty? to)) buf
        (let [r (buf :str)
              tabsize (buf :tabsize)
              c {:pos a
                 :len (- b a)
                 :to (if (buf :expandtab)
                       (expand-tab (str to)
                                   (last-tabstop (subr r 0 a))
                                   tabsize)
                       (str to))}
              [newbuf rc] (buf-apply-change buf c)
              undo (push-pending (newbuf :pending-undo) rc (buf :pos))]
          (assoc newbuf :pending-undo undo))))
  ([buf [a b] to]
    (buf-replace buf a b to)))

(defn buf-delete
  ([buf a b]
    (buf-replace buf a b ""))
  ([buf [a b]]
    (buf-delete buf a b)))

(defn buf-insert
  ([buf s]
    (let [pos (buf :pos)]
      (buf-replace buf pos pos s)))
  ([buf pos s]
    (buf-replace buf pos pos s)))

(defn buf-subr [buf a b]
  (-> buf :str (subr a b) str))

;A change is one edit at **single** point. 
;For example:
;  {:len 0 :to "aa" :pos 0}
;means insert "aa" at 0
;:pos must always be start point of changes
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
    (fn [chs c]
      ;(println chs)
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
(defn save-undo [buf]
  (let [pending (buf :pending-undo)]
    (if (empty? pending) buf
        (-> buf
            (assoc :pending-undo nil)
            (update :history
                    new-future
                    (update pending :changes (comp reverse merge-changes)))))))

(defn- apply-undo [buf undo]
  (let [chs (undo :changes)
        [newbuf rchs] (buf-apply-changes buf chs)]
    [(buf-set-pos newbuf (undo :cursor)) rchs]))

(defn undo [buf]
  (let [item (just-now (buf :history))]
    (if (nil? item) buf
        (let [[newbuf rchs] (apply-undo buf item)]
          (update newbuf :history
                  go-back (fn [item] 
                            (assoc item :changes rchs)))))))

(defn redo [buf]
  (let [item (next-future (buf :history))]
    (if (nil? item) buf
        (let [[newbuf rchs] (apply-undo buf item)]
          (update newbuf :history 
                  go-future (fn [item] 
                              (assoc item :changes rchs)))))))

(defn re-test [re s]
  (cond (nil? re) false
        (string? s) (.find (re-matcher re s))
        :else (.find (.matcher s re))))

(defn re-subs [re s]
  (cond (nil? re) nil
        (string? s) (re-find re s)
        :else (let [m (.matcher s re)]
                (if (.find m)
                  (subr s (.start m) (.end m))
                  nil))))

(defn count-left-spaces [s]
  (count (re-subs #"^\s*" s)))
;(pos-re-backward 2 (rope "aa bb") #"(?<=[^a-zA-Z_\-!.?+*=<>&#\':0-9])[a-zA-Z_\-!.?+*=<>&#\':0-9]|(?<=[a-zA-Z_\-!.?+*=<>&#\':0-9\s,])[^a-zA-Z_\-!.?+*=<>&#\':0-9\s,]")

;(let [s (rope "aa\nbb\ncc\n\n")] 
;  (take 30
;        (lines s 0 9)))
;(lines (rope "aa\nbb\ncc\n\n") 0)

(defn char-at
  "return nil if out of range"
  [s pos]
  (if (or (neg? pos) (>= pos (count s)))
    nil
    (.charAt s pos)))

(defn ranges-to-texts
  "Return a lazy seq contains texts sub from s. Range's right point is exclusive."
  [r ranges]
  (map #(subr r %) ranges))

(defn rope-seq
  ([r pos]
    (-> r (.iterator pos) iterator-seq))
  ([r]
    (-> r .iterator iterator-seq)))

(defn rope-rseq
  ([r pos]
    (-> r (.reverseIterator (- (count r) pos 1)) iterator-seq))
  ([r]
    (-> r .reverseIterator iterator-seq)))
