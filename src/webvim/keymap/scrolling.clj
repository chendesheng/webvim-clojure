(ns webvim.keymap.scrolling
  (:require [webvim.core.utils :refer [deep-merge bound-range]]
            [webvim.scrolling :refer [scroll-to viewport-center viewport-inc-lines]]
            [webvim.core.ui :refer [viewport]]
            [webvim.core.line :refer [lines-row pos-line-first pos-line-last]]
            [webvim.keymap.compile :refer [wrap-keycode]]))

(defn- not-first-line [f]
  (fn [{pos :pos r :str :as buf} keycode]
    (if (zero? (pos-line-first r pos))
      (assoc buf :beep true)
      (f buf keycode))))

(defn- end? [r pos]
  (>= (inc pos) (count r)))

(defn- not-last-line [f]
  (fn [{pos :pos r :str :as buf} keycode]
    (if (end? r (pos-line-last r pos))
      (assoc buf :beep true)
      (f buf keycode))))

(defn- round-to-zero
  "(round-to-zero -9.1) = -9; (round-to-zero 9.1) = 9"
  [i]
  (if (> i 0)
    (int i)
    (- (int (- i)))))

(defn- negzero [n]
  (if (neg? n) 0 n))

(defn- cursor-move-viewport
  "Jump cursor by viewport height, deps to window's :viewport"
  [factor]
  (fn [buf keycode]
    (let [h ((viewport) :h)
          d (round-to-zero (* h factor))
          scroll-top (buf :scroll-top)
          row (-> buf :y)
          vrow (- row scroll-top)
          newrow (bound-range (+ row d) 0 (buf :linescnt))
          newst (-> newrow (- vrow) negzero)]
      (-> buf
          (assoc :scroll-top newst)
          (lines-row newrow)))))

(defn- scrolling-keymap []
  {"z" {"z" (scroll-to viewport-center)}
   "<c-u>" (not-first-line (cursor-move-viewport -0.5))
   "<c-d>" (not-last-line (cursor-move-viewport 0.5))
   ;TODO: scroll beyond border
   "<c-e>" (scroll-to (viewport-inc-lines (- 1)))
   "<c-y>" (scroll-to (viewport-inc-lines 1))})

(defn wrap-keymap-scrolling [keymap]
  (deep-merge keymap (scrolling-keymap)))

(defn wrap-keymap-scrolling-visual [keymap]
  (deep-merge keymap (scrolling-keymap)))

;TODO: za, zt, zb <c-b> etc.
