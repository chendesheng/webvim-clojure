(ns webvim.keymap.scrolling
  (:require [webvim.core.utils :refer [deep-merge bound-range]]
            [webvim.scrolling :refer [scroll-to viewport-center viewport-inc-lines]]
            [webvim.core.ui :refer [ui-agent]]
            [webvim.core.line :refer [lines-row]]
            [webvim.keymap.compile :refer [wrap-keycode]]))

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
    (let [h (-> @ui-agent :viewport :h)
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
   "<c-u>" (cursor-move-viewport -0.5) 
   "<c-d>" (cursor-move-viewport 0.5)
   ;TODO: scroll beyond border
   "<c-e>" (scroll-to (viewport-inc-lines (- 1)))
   "<c-y>" (scroll-to (viewport-inc-lines 1))})

(defn wrap-keymap-scrolling [keymap]
  (deep-merge keymap (scrolling-keymap)))

(defn wrap-keymap-scrolling-visual [keymap]
  (deep-merge keymap (scrolling-keymap)))

;TODO: za, zt, zb <c-b> etc.
