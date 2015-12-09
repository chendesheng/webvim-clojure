(ns webvim.jumplist
  (:require [clojure.core.async :as async]
            [clojure.java.io :as io])
  (:use clojure.pprint
        webvim.core.event
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

(defonce jump-list (atom (parallel-universe)))

;jump-push before these motions
(def motions-push-jumps
  #{"/" "?" "*" "#" "n" "N" "%" "G" "<c+d>" "<c+u>" "{" "}" "gg" "gd"})

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
  (new-future jl (select-keys buf [:id :pos])))

(defn jump-push
  "Add :pos to jump list"
  [buf]
  (swap! jump-list push-current buf))

;TODO: handle not exist buffer
(defn jump-next
  [buf]
  (if (no-next? @jump-list) nil
    (-> jump-list
        (swap! go-future)
        next-future)))

(defn jump-prev
  [buf]
  (if (no-prev? @jump-list) nil
    (-> jump-list
        (swap! (fn [jl]
                 (if (no-current? jl) 
                   (-> jl
                       (push-current buf)
                       go-back
                       go-back)
                   (go-back jl))))
        next-future)))

;keep track positions when buffer changed
(defonce ^:private listen-change-buffer 
  (listen
    :change-buffer
    (fn [buf oldbuf c]
      (let [bufid (buf :id)
            cpos (c :pos)
            delta (- (-> c :to count) (c :len))]
        (swap! jump-list 
               rewrite-history (fn[{id :id pos :pos :as p}]
                                 (if (and (= id bufid) (>= pos cpos))
                                     {:id id :pos (+ pos delta)} 
                                     p)))
        buf))))
