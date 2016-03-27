(ns webvim.scrolling
  (:require [webvim.core.ui :refer [viewport]]))

(defn scroll-to [f]
  (fn [buf keycode]
    (assoc buf :scroll-top
           (f (buf :scroll-top) ((viewport) :h) (buf :y)))))

(defn viewport-center [scroll-top height cursor]
  (- cursor (int (/ height 2))))

(defn viewport-inc-lines [cnt]
  (fn [scroll-top height cursor]
    (+ scroll-top cnt)))

(defn cursor-center-viewport [buf]
  ((scroll-to viewport-center) buf nil))
