(ns webvim.action
  (:use webvim.global
        webvim.core.line))

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
  [b factor]
  (let [d (round-to-zero (* (:h (:viewport @window)) factor))
        scroll-top (b :scroll-top)
        h (-> @window :viewport :h)
        row (-> b :y)
        vrow (- row scroll-top)
        newrow (bound-range (+ row d) 0 (b :linescnt))
        newst (-> newrow (- vrow) negzero)]
    (-> b
        (assoc :scroll-top newst)
        (lines-row newrow))))

(defn cursor-center-viewport[b]
  (assoc b :scroll-top 
            (-> b :y
                (- (int (/ (-> @window :viewport :h) 2))))))

