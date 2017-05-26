(ns webvim.jumplist
  (:require [clojure.java.io :as io])
  (:use clojure.pprint
        webvim.core.event
        webvim.core.rope
        webvim.core.parallel-universe))

;;
; motion             | jumplist    | cursor after motion
;--------------------|-------------|--------------------
; initial position A | (A |)       | A 
; jump to B          | (B | A)     | B  
; jump to C          | (C | B A)   | C     
; c+o                | (C B | A)   | B      
; c+o                | (C B A |)   | A
; jump to D          | (D | A)     | D
; move to D'         | (D | A)     | D'
; c+o                | (D' A |)    | A
; c+i                | (D' | A)    | D'
; c+i                | (D' | A)    | D'
;
; zipper api
; initial: (right-most (next (zipper [pos])))
; current location: (node zp)
; jump to pos: (left (insert-left (next (zipper (cons (node zp) (rights zp)))) pos))
; c+o: (if (-> zp rights emptys) zp (right zp))
; c+i: (if (-> zp lefts empty?) zp (left zp))


;global list of history positions
;1) save position **before** motions 
;2) cursor always equal to (next-future @jumplist) if next-future is nil use buf position as next-future
;Example: 
; motion             | jumplist                         | cursor after motion
;--------------------|----------------------------------|--------------------
; initial position A | {:before ()      :after ()}      | A 
; jump to B          | {:before (A)     :after ()}      | B  
; jump to C          | {:before (B A)   :after ()}      | C     
; c+o                | {:before (A)     :after (B C)}   | B      
; c+o                | {:before ()      :after (A B C)} | A
; jump to D          | {:before (A)     :after ()}      | D
; move to D'         | {:before (A)     :after ()}      | D'
; c+o                | {:before ()      :after (A D')}  | A
; c+i                | {:before (A)     :after (D')}    | D'
; c+i                | {:before (A)     :after (D')}    | D'

(listen :create-window
        (fn [window]
          (println "jumplist create-window")
          (assoc window
                 :jumplist {})))

(defn jumplist-before [jumplist]
  (jumplist :before))

;jump-push before these motions
;ONLY work for single keycode
(def motions-push-jumps
  #{"/" "?" "*" "#" "n" "N" "%" "G" "{" "}" "H" "M" "L"})

(defn- no-next? [jl]
  (-> jl
      go-future
      next-future
      nil?))

(defn- no-prev? [jl]
  (-> jl
      just-now
      nil?))

;just-now is prev
;next-future is current
;next-future next-future is next
(defn- no-current? [jl]
  (-> jl
      next-future
      nil?))

(defn- push-current [jl buf]
  (-> jl
      (new-future (select-keys buf [:id :pos :y :filepath :name]))))

(defn- update-jumplist [buf fn-update]
  (update-in
    buf
    [:window :jumplist] fn-update))

(defn jump-push
  "Add :pos to jump list"
  [{id :id pos :pos :as buf}]
  (update-jumplist
    buf
    (fn [jl]
      (if (and
            (= (-> jl just-now :pos) pos)
            (= (-> jl just-now :id) id))
        jl
        (push-current jl buf)))))

(defn jump-next
  [buf]
  (update-jumplist buf go-future))

(defn jump-prev [buf]
  (update-jumplist
    buf
    (fn [jl]
      (if (-> jl next-future nil?)
        (-> jl
            (push-current buf)
            go-back
            go-back)
        (go-back jl)))))

(defn jump-current-pos [buf]
  (-> buf :window :jumplist next-future))

;split old buffer to 3 parts [0 a) [a b) [b count)
(defn- shift-by-change [pos y oldbuf {cpos :pos clen :len :as c}]
  (let [a cpos
        b (+ a clen)]
    (cond
      (< pos a)
      {:pos pos :y y}
      (<= b pos)
      (let [del-len clen
            ins-len (-> c :to count)
            del-y (-> oldbuf :str (subr cpos (+ cpos clen)) count-<br>)
            ins-y (-> c :to count-<br>)]
        {:pos (+ pos (- ins-len del-len)) :y (+ y (- ins-y del-y))})
      :else
      {:pos (-> c :to count (+ a)) :y (-> c :to count-<br> (+ y))})))

(defn- on-buffer-change [buf oldbuf c]
  (let [bufid (buf :id)]
    ;TODO: This could be slow.
    (update-jumplist
      buf
      (fn [jl]
        (rewrite-history
          jl
          (fn [{id :id pos :pos y :y :as p}]
            (if (= id bufid)
              (merge p (shift-by-change pos y oldbuf c))
              p)))))))

;keep track positions when buffer changed
(listen
  :change-buffer
  (fn [buf oldbuf c]
    (on-buffer-change buf oldbuf c)))
