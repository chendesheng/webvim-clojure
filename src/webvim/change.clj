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
;Reverse changes simply swap :len and :to:
;  ~{:len 3 :to "bb" :pos 0} => {:len 2 :to "aaa" :pos 0}
;
;History: [c1 c2 c3*]
;  undo:  [c1 c2* c3] apply ~c3
;  redo:  [c1 c2 c3*] apply c3

(defn apply-changes[b changes]
  ())

(defn insert?[{to :to len :len}]
  (and (not (empty? to)) (zero? len)))

(defn delete?[{to :to len :len}]
  (and (empty? to) (pos? len)))

(defn replace?[{to :to len :len}]
  (and (not (empty? to)) (pos? len)))

(defn unchange?[{to :to len :len}]
  (and (empty? to) (zero? len)))

(defn changes-merge
  "s is text **before** changes"
  [s changes]
  (reduce 
    (fn[chs c]
      (if (vector? chs)
        (let [lastc (peek chs)]
          ;conditions: lastc is delete, 
          (conj chs c))
        [c]) changes)))

(defn- merge-change
  "return nil if can't merge otherwise return merged change"
  [c1 c2]
  (let [p1 (c1 :pos)
        p2 (c2 :pos)
        to1 (c1 :to)
        to2 (c2 :to)
        l1 (c1 :len)
        l2 (c2 :len)
        lt1 (count to1)
        lt2 (count to2)]
  (cond 
    (insert? c1)
    (cond 
      (insert? c2)
      (cond 
        (= p1 p2)
        {:pos p1
         :to (str to2 to1)
         :len 0}
        (= (+ p1 lt1) p2)
        {:pos p1
         :to (str to1 to2)
         :len 0}
        :else
        nil)
      (delete? c2)
      (cond 
        (= p1 p2)
        {:pos p1
         :to (subs to1 (min l2 lt1))
         :len (max 0 (- l2 lt1))}
        (= (+ p1 lt1) p2)
        {:pos p1
         :to to1
         :len l2}
        (= p1 (+ p2 l2))
        {:pos p2
         :to to1
         :len l2}
        :else
        nil)
      (replace? c2)
      (cond 
        (= p1 p2)
        {:pos p1
         :to (str to2 (subs to1 (min l2 lt1)))
         :len (max 0 (- l2 lt1))}
        (= (+ p1 lt1) p2)
        {:pos p1
         :to (str to1 to2)
         :len l2}
        (= p1 (+ p2 l2))
        {:pos p2
         :to (str to2 to1)
         :len l2}
        :else
        nil)
      (unchange? c2)
      c1)
    (delete? c1)
    (cond 
      (unchange? c2)
      c1
      (and (= p1 p2))
      {:pos p1
       :to to2
       :len (+ l1 l2)}
      :else nil)
    (replace? c1)
    (cond 
      (insert? c2)
      (cond 
        (= p1 p2)
        {:pos p1
         :to (str to2 to1)
         :len l1}
        (= (+ p1 lt1) p2)
        {:pos p1
         :to (str to1 to2)
         :len l1}
        :else nil)
      (delete? c2)
      (cond
        (= p1 p2)
        {:pos p1
         :to (subs to1 (min lt1 l2))
         :len (+ l1 (max 0 (- lt1 l2)))}
        (= p1 (+ p2 l2))
        {:pos p2
         :to to1
         :len (+ l1 l2)}
        (= (+ p1 lt1) p2)
        {:pos p1
         :to to1
         :len (+ l1 l2)}
        :else nil)
      (replace? c2)
      (cond
        (= p1 p2)
        {:pos p1
         :to (str to2 (subs to1 (min lt1 l2)))
         :len (+ l1 (max 0 (- lt1 l2)))}
        (= p1 (+ p2 l2))
        {:pos p2
         :to (str to2 to1)
         :len (+ l1 l2)}
        (= (+ p1 lt1) p2)
        {:pos p1
         :to (str to1 to2)
         :len (+ l1 l2)}
        :else nil)
      (unchange? c2)
      c1)
    (unchange? c1)
    c2)))

;test cases
;insert + insert
(merge-change {:pos 0 :len 0 :to "aa"} {:pos 2 :len 0 :to "bb"})
(merge-change {:pos 0 :len 0 :to "aa"} {:pos 0 :len 0 :to "bb"})

;insert + delete
(merge-change {:pos 0 :len 0 :to "aa"} {:pos 2 :len 1 :to ""})
(merge-change {:pos 5 :len 0 :to "aa"} {:pos 2 :len 3 :to ""})
(merge-change {:pos 0 :len 0 :to "aa"} {:pos 0 :len 1 :to ""})
(merge-change {:pos 0 :len 0 :to "aa"} {:pos 0 :len 3 :to ""})

;insert + replace
(merge-change {:pos 0 :len 0 :to "aa"} {:pos 2 :len 3 :to "bb"}) ; =>  {:pos 0 :len 3 :to "aabb"}
(merge-change {:pos 5 :len 0 :to "aa"} {:pos 2 :len 3 :to "bb"}) ; =>  {:pos 2 :len 3 :to "bbaa"}
(merge-change {:pos 0 :len 0 :to "aa"} {:pos 0 :len 3 :to "bb"}) ; =>  {:pos 0 :len 1 :to "bb"}
(merge-change {:pos 0 :len 0 :to "aa"} {:pos 0 :len 5 :to "bb"}) ; =>  {:pos 0 :len 3 :to "bb"}

;delete + insert
(merge-change {:pos 0 :len 1 :to ""} {:pos 0 :len 0 :to "aa"})
(merge-change {:pos 0 :len 3 :to ""} {:pos 0 :len 0 :to "aa"})
(merge-change {:pos 0 :len 1 :to ""} {:pos 0 :len 0 :to "aa"})
(merge-change {:pos 0 :len 3 :to ""} {:pos 0 :len 0 :to "aa"})
(merge-change {:pos 0 :len 3 :to ""} {:pos 0 :len 4 :to ""})
(merge-change {:pos 0 :len 3 :to ""} {:pos 0 :len 4 :to "bb"})
(merge-change {:pos 0 :len 3 :to ""} {:pos 1 :len 4 :to "bb"})

(defn change-reverse[change]
  {:len (change :to) :to (change :len) :pos (change :pos)})
