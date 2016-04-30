(ns webvim.core.range
  (:require [webvim.core.utils :refer [sort2]]
            [webvim.core.line :refer :all]))

(defn range-inclusive [rg]
  (let [[a b] (sort2 rg)]
    [a (dec b)]))

(defn range-exclusive [rg]
  (let [[a b] (sort2 rg)]
    [a (inc b)]))

(defn range-linewise
  ([buf a b]
    (let [[a b] (sort2 a b)]
      [(pos-line-first buf a)
       (pos-line-last buf b)]))
  ([buf [a b]]
    (range-linewise buf a b)))

(defn range-line-end [buf pos]
  [pos (pos-line-end buf pos)])

(defn range-current-line [buf pos]
  [(pos-line-start buf pos)
   (pos-line-end buf pos)])
