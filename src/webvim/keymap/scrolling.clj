(ns webvim.keymap.scrolling
  (:require [webvim.core.utils :refer [deep-merge bound-range keymap-comp]]
            [webvim.scrolling :refer [scroll-to viewport-center viewport-inc-lines viewport-top viewport-bottom]]
            [webvim.core.rope :refer [buf-total-lines]]
            [webvim.core.line :refer [lines-row line-start column]]
            [webvim.keymap.motion :refer [not-last-line not-first-line not-scroll-start wrap-repeat]]
            [webvim.keymap.compile :refer [wrap-keycode]]))

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
    (let [h (-> buf :window :viewport :h)
          d (round-to-zero (* h factor))
          scroll-top (buf :scroll-top)
          row (-> buf :y)
          vrow (- row scroll-top)
          newrow (bound-range (+ row d) 0 (buf-total-lines buf))
          newst (-> newrow (- vrow) (max 0))]
      (-> buf
          (assoc :scroll-top newst)
          (lines-row newrow)
          line-start))))

(defn- forward-page [buf keycode]
  (-> buf
      (lines-row (-> buf
                     :scroll-top
                     (+ (-> buf :window :viewport :h))
                     (- 2)))
      line-start))

(defn- scroll-end? [buf]
  (>= (-> buf :scroll-top inc) (buf-total-lines buf)))

(defn- backward-page [{scroll-top :scroll-top :as buf} keycode]
  (let [newy (if (scroll-end? buf)
               (dec scroll-top)
               (inc scroll-top))]
    (-> buf
        (lines-row newy)
        line-start)))

(defn- <c-f>-beep? [f]
  (fn [buf keycode]
    (if (scroll-end? buf)
      ((not-last-line f) buf keycode)
      (f buf keycode))))

(defn- scrolling-keymap []
  {"z" {"z" (scroll-to viewport-center)
        "t" (scroll-to viewport-top)
        "b" (scroll-to viewport-bottom)}
   "<c-u>" (wrap-repeat (not-first-line (cursor-move-viewport -0.5)))
   "<c-d>" (wrap-repeat (not-last-line (cursor-move-viewport 0.5)))
   "<c-f>" (wrap-repeat (<c-f>-beep? (keymap-comp (scroll-to viewport-top) forward-page)))
   "<c-b>" (wrap-repeat (not-scroll-start (keymap-comp (scroll-to viewport-bottom) backward-page)))
   ;TODO: scroll beyond border
   "<c-e>" (wrap-repeat (scroll-to (viewport-inc-lines (- 1))))
   "<c-y>" (wrap-repeat (scroll-to (viewport-inc-lines 1)))})

(defn wrap-keymap-scrolling [keymap]
  (deep-merge keymap (scrolling-keymap)))

(defn wrap-keymap-scrolling-visual [keymap]
  (deep-merge keymap (scrolling-keymap)))

;TODO: za, <c-b> etc.
