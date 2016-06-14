(ns webvim.keymap.scrolling
  (:require [webvim.core.utils :refer [deep-merge bound-range negzero]]
            [webvim.scrolling :refer [scroll-to viewport-center viewport-inc-lines viewport-top viewport-bottom]]
            [webvim.core.ui :refer [viewport]]
            [webvim.core.rope :refer [buf-total-lines]]
            [webvim.core.line :refer [lines-row pos-line-first pos-line-last line-start column]]
            [webvim.keymap.compile :refer [wrap-keycode]]))

(defn- not-first-line [f]
  (fn [buf keycode]
    (if (zero? (pos-line-first buf))
      (assoc buf :beep true)
      (f buf keycode))))

(defn- end? [r pos]
  (>= (inc pos) (count r)))

(defn- not-last-line [f]
  (fn [{r :str :as buf} keycode]
    (if (end? r (pos-line-last buf))
      (assoc buf :beep true)
      (f buf keycode))))

(defn- round-to-zero
  "(round-to-zero -9.1) = -9; (round-to-zero 9.1) = 9"
  [i]
  (if (> i 0)
    (int i)
    (- (int (- i)))))

(defn- cursor-move-viewport
  "Jump cursor by viewport height, deps to window's :viewport"
  [factor]
  (fn [buf keycode]
    (let [h ((viewport) :h)
          d (round-to-zero (* h factor))
          scroll-top (buf :scroll-top)
          row (-> buf :y)
          vrow (- row scroll-top)
          newrow (bound-range (+ row d) 0 (buf-total-lines buf))
          newst (-> newrow (- vrow) negzero)]
      (-> buf
          (assoc :scroll-top newst)
          (lines-row newrow)
          line-start))))

(defn- scrolling-keymap []
  {"z" {"z" (scroll-to viewport-center)
        "t" (scroll-to viewport-top)
        "b" (scroll-to viewport-bottom)}
   "<c-u>" (not-first-line (cursor-move-viewport -0.5))
   "<c-d>" (not-last-line (cursor-move-viewport 0.5))
   ;TODO: scroll beyond border
   "<c-e>" (scroll-to (viewport-inc-lines (- 1)))
   "<c-y>" (scroll-to (viewport-inc-lines 1))})

(defn wrap-keymap-scrolling [keymap]
  (deep-merge keymap (scrolling-keymap)))

(defn wrap-keymap-scrolling-visual [keymap]
  (deep-merge keymap (scrolling-keymap)))

;TODO: za, <c-b> etc.
