(ns webvim.cursor
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [clojure.java.io :as io])
  (:use clojure.pprint
        (clojure [string :only (join split)])
        webvim.autocompl
        webvim.history
        webvim.line
        webvim.global))

(defn round-to-zero
  "(round-to-zero -9.1) = -9; (round-to-zero 9.1) = 9"
  [i]
  (if (> i 0)
    (int i)
    (- (int (- i)))))

(defn negzero[n]
  (if (neg? n) 0 n))

(defn cursor-move-viewport
  "Jump cursor by viewport height, deps to window's :viewport"
  [b factor]
  (let [d (round-to-zero (* (:h (:viewport @window)) factor))
        scroll-top (b :scroll-top)
        h (-> @window :viewport :h)
        row (-> b :y)
        vrow (- row scroll-top)
        newrow (bound-range (+ row d) 0 (b :linescnt))
        newst (-> newrow (- vrow) negzero)]
    (-> b
        (assoc :scroll-top newst)
        (lines-row newrow))))

(defn cursor-center-viewport[b]
  (assoc b :scroll-top 
            (-> b :y
                (- (int (/ (-> @window :viewport :h) 2))))))
