(ns webvim.test-util
  (:require [clojure.test :refer :all]
            [webvim.buffer :refer :all])
  (:use clojure.pprint))

(defn check-cursor [b [r c lc vr]]
  (and (= r (-> b :cursor :row))
       (= c (-> b :cursor :col))
       (= lc (-> b :cursor :lastcol))
       (= vr (-> b :cursor :vprow))))

(defn check-range [b [[r1 c1] [r2 c2]]]
  (and (= r1 (((-> b :visual :ranges) 0) :row))
       (= c1 (((-> b :visual :ranges) 0) :col))
       (= r2 (((-> b :visual :ranges) 1) :row))
       (= c2 (((-> b :visual :ranges) 1) :col))))

(defn testprint[obj]
  (pprint obj)
  obj)

(def empty-buf (create-buf "empty-buf" ""))
