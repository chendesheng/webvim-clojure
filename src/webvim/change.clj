(ns webvim.change
  (:use webvim.global
        webvim.text))

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
;
;History: [c1 c2 c3*]
;  undo:  [c1 c2* c3] apply ~c3
;  redo:  [c1 c2 c3*] apply c3

(defn changes-merge
  [changes]
  (reduce 
    (fn[chs c]
      (if (vector? chs)
        (let [merged (merge-change (peek chs) c)]
          (if (nil? merged)
            (conj chs c)
            (conj (pop chs) merged))))
        [c]) changes))

(defn- merge-change
  "return nil if can't merge otherwise return merged change"
  [c1 c2]
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
      :else nil)))

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

(defn apply-change[s c]
  (let [{pos :pos
         len :len
         to :to} c
        news (str-replace s pos (+ pos len) to)]
    [news {:pos pos
           :len (count to)
           :to (text-subs s pos (+ pos len))}]))

(defn apply-changes[s changes]
  (reduce 
    (fn [[s rchs] c]
      (let [[news rc] (apply-change s c)]
        [news (conj rchs rc)])) [s []] changes))

