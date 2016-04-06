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
  ([r a b]
    (let [[a b] (sort2 a b)]
      [(pos-line-first r a) (pos-line-last r b)]))
  ([r [a b]]
    (range-linewise r a b)))

(defn range-line-end [r pos]
  [pos (pos-line-end r pos)])

(defn range-current-line [r pos]
  [(pos-line-start r pos) (pos-line-end r pos)])
