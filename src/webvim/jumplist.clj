(ns webvim.jumplist
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [clojure.java.io :as io])
  (:use clojure.pprint
        webvim.global))

;global list of history positions
;jump-list behaves different from vim's jump list: 
; 1) save position **before** motions 
; 2) history position could be changed if insert text between jump back or forth
;Example: 
; Motions            | @jump-list                       | cursor  
;--------------------|----------------------------------|---------
; initial position A | {:current 0 :positions []}       | A 
; jump to B          | {:current 1 :positions [A]}      | B  
; jump to C          | {:current 2 :positions [A B]}    | C     
; c+o                | {:current 1 :positions [A B C]}  | B      
; move left to B`    | {:current 1 :positions [A B` C]} | B`   (history changed)
; c+o                | {:current 0 :positions [A B` C]} | A
; jump to D          | {:current 1 :positions [A D]}    | D
;
; FIXME:
; pros: can always jump back to exact position where jump starts
; cons: little confuse?
(defonce jump-list (atom {:current 0 ;index of next position in positions vector
                          :positions [] ;{:id :pos}
                          }))

;jump-push before these motions
(def motions-push-jumps
  #{"/" "?" "*" "#" "n" "N" "%" "G" "c+d" "c+u" "{" "}"})

(defn jump-current-pos[]
  ((@jump-list :positions) (@jump-list :current)))

;TODO: handle not exist buffer
(defn jump-next
  "If not newest, save current position then jump to next"
  [b]
  (let [{current :current
         positions :positions} @jump-list]
    (if (>= current (dec (count positions)))
      nil
      (let [bpos {:id (b :id) :pos (b :pos)}]
        (swap! jump-list assoc-in [:positions current] bpos)
        (swap! jump-list update-in [:current] inc)
        (jump-current-pos)))))

(defn jump-prev
  "If not oldest, save current position then jump to prev"
  [b]
  (let [current (@jump-list :current)]
    (if (zero? current)
      nil
      (let [bpos {:id (b :id) :pos (b :pos)}]
        (swap! jump-list assoc-in [:positions current] bpos)
        (swap! jump-list update-in [:current] dec)
        (jump-current-pos)))))

(defn jump-push
  "Add :pos to jump list"
  [b]
  (let [jl @jump-list
        positions (conj 
                    (subvec (jl :positions) 0 (jl :current))
                    {:id (:id b) :pos (:pos b)})]
    (reset! jump-list {:positions positions :current (count positions)})))

