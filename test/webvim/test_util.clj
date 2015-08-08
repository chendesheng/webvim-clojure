(ns webvim.test-util
  (:require [clojure.test :refer :all]
            [webvim.buffer :refer :all])
  (:use clojure.pprint))

(defn check-cursor [cur [r c lc vr]]
  (and (= r (cur :row))
       (= c (cur :col))
       (= lc (cur :lastcol))))

(defn check-range [ranges [[r1 c1] [r2 c2]]]
  (and (= r1 ((ranges 0) :row))
       (= c1 ((ranges 0) :col))
       (= r2 ((ranges 1) :row))
       (= c2 ((ranges 1) :col))))

(defn testprint[obj]
  (pprint obj)
  obj)

(def empty-buf (create-buf "empty-buf" ""))
