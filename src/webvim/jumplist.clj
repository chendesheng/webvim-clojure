(ns webvim.jumplist
  (:require [clojure.java.io :as io]
            [clojure.zip :as zip])
  (:use clojure.pprint
        webvim.core.event
        webvim.core.rope))

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

(defn- seq-silblings [z]
  (concat
    (zip/lefts z)
    [(zip/node z)]
    (zip/rights z)))

(defn jumplist [buf]
  (-> buf :window :jumplist seq-silblings))

;jump-push before these motions
;ONLY work for single keycode
(def motions-push-jumps
  #{"/" "?" "*" "#" "n" "N" "%" "G" "{" "}" "H" "M" "L"})

(defn- update-jumplist [buf fn-update]
  (update-in
    buf
    [:window :jumplist] fn-update))

(defn- get-location [buf]
  (select-keys buf [:id :pos :y :filepath :name]))

(defn jump-push [buf]
  (let [loc (get-location buf)]
    (update-jumplist
      buf
      (fn[jl]
        (->> (zip/rights jl)
             (cons loc)
             (cons nil)
             zip/seq-zip
             zip/next)))))

; prevent nil location
(defn- safe-move [location fn-move]
  (or (fn-move location) location))

(defn jump-next [buf]
  (update-jumplist buf #(safe-move % zip/left)))

(defn jump-prev [buf]
  (update-jumplist
    buf
    (fn[jl]
      (-> jl
          (zip/edit #(or % (get-location buf)))
          (safe-move zip/right)))))

(defn jump-current-pos [buf]
  (or
    (-> buf :window :jumplist zip/node)
    (get-location buf)))

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
        (let [fn-update (fn [{id :id pos :pos y :y :as p}]
                            (if (= id bufid)
                              (merge p (shift-by-change pos y oldbuf c))
                              p))]
          (->> jl
              zip/rights
              (map fn-update)
              (cons (fn-update (zip/node jl)))
              zip/seq-zip
              zip/next))))))

;keep track positions when buffer changed
(listen
  :change-buffer
  (fn [buf oldbuf c]
    (on-buffer-change buf oldbuf c)))
