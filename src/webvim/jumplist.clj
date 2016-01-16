(ns webvim.jumplist
  (:require [clojure.core.async :as async]
            [clojure.java.io :as io])
  (:use clojure.pprint
        webvim.core.event
        webvim.core.rope
        webvim.core.parallel-universe))

;global list of history positions
;1) save position **before** motions 
;2) cursor always equal to (next-future @jump-list) if next-future is nil use buf position as next-future
;Example: 
; motion             | @jump-list                       | cursor after motion
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

(defonce jump-list (agent (parallel-universe)))

;jump-push before these motions
;ONLY work for single keycode
(def motions-push-jumps
  #{"/" "?" "*" "#" "n" "N" "%" "G" "<c+d>" "<c+u>" "{" "}"})

(defn- no-next?[jl]
  (-> jl
      go-future
      next-future
      nil?))

(defn- no-prev?[jl]
  (-> jl
      just-now
      nil?))

;just-now is prev
;next-future is current
;next-future next-future is next
(defn- no-current?[jl]
  (-> jl
      next-future
      nil?))

(defn- push-current[jl buf]
  (new-future jl (select-keys buf [:id :pos :y :filepath :name])))

(defn jump-push
  "Add :pos to jump list"
  [buf]
  (send jump-list (fn[jl {id :id pos :pos :as buf}]
                     (if (and
                           (= (-> jl just-now :pos) pos)
                           (= (-> jl just-now :id) id))
                       jl
                       (push-current jl buf))) buf)
  buf)

;TODO: handle not exist buffer
(defn jump-next
  [buf]
  (if (no-next? @jump-list)
    nil
    (let [res (-> @jump-list
                  go-future
                  next-future)]
          (send jump-list go-future)
          res)))

(defn jump-prev
  [buf]
  (if (no-prev? @jump-list)
    nil
    (do
      (let [res (just-now @jump-list)]
        (send jump-list (fn [jl]
                          (if (no-current? jl) 
                            (-> jl
                                (push-current buf)
                                go-back
                                go-back)
                            (go-back jl))))
        res))))

(defn- shift-pos[pos y cpos del-len ins-len del-y ins-y]
  (let [a cpos
        b (+ a del-len)]
    ;split old buffer to 3 parts [0 a) [a b) [b count)
    (cond 
      (< pos a)
      {:pos pos :y y}
      (and (<= a pos) (< pos b))
      {:pos (+ a ins-len) :y (+ y ins-y)}
      (<= b pos)
      {:pos (+ pos (- ins-len del-len)) :y (+ y (- ins-y del-y))})))

(defn- shift-by-change[pos y oldbuf {cpos :pos clen :len :as c}]
  (shift-pos pos y cpos clen (-> c :to count)
             (-> oldbuf :str (subr cpos (+ cpos clen)) count-<br>)
             (-> c :to count-<br>)))

(defn- on-buffer-change[buf oldbuf c]
  (let [bufid (buf :id)]
    (send jump-list 
           rewrite-history
           (fn[{id :id pos :pos y :y :as p}]
             (if (= id bufid)
               (merge p (shift-by-change pos y oldbuf c))
               p)))
    buf))

;keep track positions when buffer changed
(defonce ^:private listen-change-buffer 
  (listen
    :change-buffer
    (fn [buf oldbuf c]
      (on-buffer-change buf oldbuf c))))
