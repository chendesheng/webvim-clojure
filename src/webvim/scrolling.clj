(ns webvim.scrolling)

(defn scroll-to [f]
  (fn [buf keycode]
    (println "scroll-to: viewport=" (-> buf :window :id) (-> buf :window :viewport))
    (assoc buf :scroll-top
           (f (buf :scroll-top) (-> buf :window :viewport :h) (buf :y)))))

(defn viewport-center [scroll-top height y]
  (- y (int (/ height 2))))

(defn viewport-bottom [scroll-top height y]
  (- y height))

(defn viewport-top [scroll-top height y]
  y)

(defn viewport-inc-lines [cnt]
  (fn [scroll-top height y]
    (+ scroll-top cnt)))

(defn cursor-center-viewport [buf]
  ((scroll-to viewport-center) buf nil))
