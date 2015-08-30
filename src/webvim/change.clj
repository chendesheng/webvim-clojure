(ns webvim.change)

;A change is one edit at **single** point. 
;For example:
;  {:from "" :to "aa" :pos 0}
;means insert "aa" at 0
;:pos must always be left point of changes
;
;Merge changes (changes can be merged into **one** :pos):
;  1) {:from "aaa" :to "" :pos 0} + {:from "" :to "bb" :pos 0} =>  {:from "aaa" :to "bb" :pos 0}
;  2) {:from "" :to "aa" :pos 0} + {:from "" :to "bb" :pos 2} => {:from "" :to "aabb" :pos 0}
;  3) {:from "abc" :to "" :pos 0} + {:from "def" :to "" :pos 0} => {:from "abcdef" :to "" :pos 0}
;  4) {:from "" :to "abc" :pos 0} + {:from "c" :to "" :pos 2} => {:from "" :to "ab" :pos 0}
;  ...
;Reverse changes simply swap :from and :to:
;  ~{:from "aaa" :to "bb" :pos 0} => {:from "bb" :to "aaa" :pos 0}
;
;History: [c1 c2 c3*]
;  undo:  [c1 c2* c3] apply ~c3
;  redo:  [c1 c2 c3*] apply c3

(defn apply-changes[b changes]
  ())

(defn change-merge[& changes]
  ())

(defn change-reverse[change]
  {:from (change :to) :to (change :from) :pos (change :pos)})
