(ns webvim.line
  (:use clojure.pprint
        webvim.change
        webvim.text
        webvim.global))

(def re-line-break (re-pattern line-break))

(defn pos-line[s pos]
  (let [b (or (last (pos-re-forward pos s #"\n")) (count s))
        a (or (last (pos-re-next-backward pos s #"\n")) 0)]
    [a b]))

(defn current-line
  [t]
   "return range of line under cursor, right side is exclusive"
   (pos-line (t :str) (t :pos)))

;(current-line {:pos 1 :str (text-new "\naaa\n")})
;(current-line {:pos 5 :str (text-new "\nbb\n\naaa\n")})

(defn pos-line-first[pos s]
  (pos-re pos s #"((?<=\n)[\s\S])" pos-re-backward 0))

(defn line-first[t]
  (let [s (t :str)
        pos (t :pos)
        newpos (pos-line-first pos s)]
    (text-update-pos t newpos)))

(defn pos-line-start[pos s]
  (-> pos
      (pos-line-first s)
      (pos-re s #"[\S\n]|((?<=\s)[\S\n])" pos-re-forward)))

(defn line-start[t]
  (let [newpos (pos-line-start (t :pos) (t :str))]
    (text-update-pos t newpos)))

(defn line-end[t]
  (text-re t #"\n" pos-re-forward (-> t :str count dec)))

(defn lines-reverse
  "return a lazy seq, line by line start at pos back to top"
  [s pos]
  (let [rg (pos-line s pos)
        [a _] rg]
    (if (pos? a)
      (cons rg (lazy-seq (lines-reverse s (dec a))))
      (list rg))))

(defn lines
  "return a lazy seq, line by line start at pos until bottom"
  ([s pos]
   (let [rg (pos-line s pos)
         [_ b] rg]
     (if (< b (count s))
       (cons rg (lazy-seq (lines s b)))
       (list rg))))
  ([s] (lines s 0))
  ([s p1 p2] ;both inclusive
   (take-while
     #(>= p2 (first %))
     (lines s p1))))

;(let [s (text-new "aa\nbb\ncc\n\n")] 
;  (take 30
;        (lines s 0 9)))
;(lines (text-new "aa\nbb\ncc\n\n") 0)

(defn- lines-move[t n fndir]
  (let [pos (t :pos)
        s (t :str)
        x (t :x)
        rg (nth (fndir s pos) n nil)]
    (println rg)
    (if (nil? rg) t
      (let [[a b] rg
            newpos (+ a (bound-range x 0 (- b a 1)))]
        (println newpos)
        (text-update-pos t newpos)))))

(defn lines-forward[t n]
  (lines-move t n lines))

;(lines-forward {:pos 0 :x 2 :str (text-new "abc\n\n") :y 0} 1)

(defn lines-backward[t n]
  (lines-move t n lines-reverse))

(defn lines-row[t n]
  (let [y (t :y)
        dy (- n y)]
    (if (pos? dy)
      (lines-forward t dy)
      (lines-backward t (- dy)))))


;(lines (text-new "aa\nbb\ncc\n\n") 0 0)

(defn pos-next-line
"Find first line pred is true start at next line"
  ([s pos pred]
   (first 
     (filter pred (rest (lines s pos)))))
  ([s pos]
   (second (lines s pos))))

;(pos-next-line (text-new "\n\n") 0)
;(pos-prev-line (text-new "\n\n") 1)

(defn pos-prev-line
  [s pos]
  (second (lines-reverse s pos)))

(defn pos-first-line
  "Find first line pred is true start at current line"
  [s pos pred]
  (first 
    (filter pred (lines s pos))))

(defn text-insert-line-after[t]
  (let [pos (t :pos)
        s (t :str)
        [_ b] (current-line t)]
    (-> t
        (text-insert b line-break)
        (text-update-pos b))))

(defn text-insert-line-before[t]
  (let [pos (t :pos)
        s (t :str)
        [a b] (current-line t)]
    (if (zero? a)
      (-> t
          (text-insert 0 line-break)
          (text-update-pos 0))
      (-> t
          (text-update-pos (- a 1))
          (text-insert line-break)))))
