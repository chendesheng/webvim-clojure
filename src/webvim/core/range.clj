(ns webvim.core.range
  (:require [webvim.core.utils :refer [sort2]]
            [webvim.core.line :refer :all]))

(defn range-inclusive [rg]
  (let [[a b] (sort2 rg)]
    [a (dec b)]))

(defn range-exclusive [rg]
  (let [[a b] (sort2 rg)]
    [a (inc b)]))

;exclusive
(defn range-linewise
  ([buf a b]
    (let [[a b] (sort2 a b)]
      [(pos-line-first buf a)
       (inc (pos-line-last buf b))]))
  ([buf [a b]]
    (range-linewise buf a b)))

(defn range-line-end [buf]
  [(buf :pos) (pos-line-end buf)])

(defn range-current-line [buf]
  [(pos-line-start buf)
   (pos-line-end buf)])
