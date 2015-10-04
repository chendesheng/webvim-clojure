(ns webvim.action.window
  (:use webvim.core.line))

;one server only serve one window at one time
(defonce window (atom{:viewport {:w 0 :h 0}}))

(defn round-to-zero
  "(round-to-zero -9.1) = -9; (round-to-zero 9.1) = 9"
  [i]
  (if (> i 0)
    (int i)
    (- (int (- i)))))

(defn negzero[n]
  (if (neg? n) 0 n))

(defn cursor-move-viewport
  "Jump cursor by viewport height, deps to window's :viewport"
  [buf factor]
  (let [d (round-to-zero (* (:h (:viewport @window)) factor))
        scroll-top (buf :scroll-top)
        h (-> @window :viewport :h)
        row (-> buf :y)
        vrow (- row scroll-top)
        newrow (bound-range (+ row d) 0 (buf :linescnt))
        newst (-> newrow (- vrow) negzero)]
    (-> buf
        (assoc :scroll-top newst)
        (lines-row newrow))))

(defn cursor-center-viewport[buf]
  (assoc buf :scroll-top 
            (-> buf :y
                (- (int (/ (-> @window :viewport :h) 2))))))

