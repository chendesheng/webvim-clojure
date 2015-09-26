(ns webvim.core.line
  (:use clojure.pprint
        webvim.core.rope
        webvim.core.pos))

(defn pos-line[r pos]
  (let [a (first (pos-re- pos r #"(?m)^"))
        b (or (first (pos-re+ (inc pos) r #"(?m)^")) (count r))]
    [a b]))

(defn -lines
  "return a lazy seq, line by line start at pos back to top"
  [r pos]
  (let [rg (pos-line r pos)
        [a _] rg]
    (if (pos? a)
      (cons rg (lazy-seq (-lines r (dec a))))
      (list rg))))

(defn lines
  "return a lazy seq, line by line start at pos until bottom"
  ([r pos]
   (let [rg (pos-line r pos)
         [_ b] rg]
     (if (< b (count r))
       (cons rg (lazy-seq (lines r b)))
       (list rg))))
  ([r] (lines r 0))
  ([r p1 p2] ;both inclusive
   (take-while
     #(>= p2 (first %))
     (lines r p1))))


;(pos-line (rope "aa\nbb") 1)

(def re-line-break (re-pattern <br>))
(defn bound-range[v r e]
  (cond (<= v r) r
        (>= v e) e
        :else v))

(defn- lines-move[t n fndir]
  (let [pos (t :pos)
        r (t :str)
        x (t :x)
        rg (nth (fndir r pos) n nil)]
    ;(println rg)
    (if (nil? rg) t
      (let [[a b] rg
            newpos (+ a (bound-range x 0 (- b a 1)))]
        ;(println newpos)
        (buf-set-pos t newpos)))))

(defn lines-forward[t n]
  (lines-move t n lines))

;(lines-forward {:pos 0 :x 2 :str (rope "abc\n\n") :y 0} 1)

(defn lines-backward[t n]
  (lines-move t n -lines))

(defn lines-row[t n]
  (let [y (t :y)
        dy (- n y)]
    (if (pos? dy)
      (lines-forward t dy)
      (lines-backward t (- dy)))))

;(lines (rope "aa\nbb\ncc\n\n") 0 0)

(defn pos-next-line
"Find first line pred is true start at next line"
  ([r pos pred]
   (first 
     (filter pred (rest (lines r pos)))))
  ([r pos]
   (second (lines r pos))))

;(pos-next-line (rope "\n\n") 0)
;(pos-prev-line (rope "\n\n") 1)

(defn pos-prev-line
  [r pos]
  (second (-lines r pos)))

(defn pos-first-line
  "Find first line pred is true start at current line"
  [r pos pred]
  (first 
    (filter pred (lines r pos))))

(defn current-line
  [t]
   "return range of line under cursor, right side is exclusive"
   (pos-line (t :str) (t :pos)))

;(current-line {:pos 1 :str (rope "\naaa\n")})
;(current-line {:pos 5 :str (rope "\nbb\n\naaa\n")})

(defn pos-line-first[pos r]
  (or (first (pos-re- pos r #"(?m)^")) 0))

;(pos-line-first 4 (rope "aa\naaa"))

(defn line-first[t]
  (let [r (t :str)
        pos (t :pos)
        newpos (pos-line-first pos r)]
    (buf-set-pos t newpos)))

(defn pos-line-start[pos r]
  (or (first
        (-> pos
            (pos-line-first r)
            (pos-re+ r #"[\S\n]|((?<=\s)[\S\n])"))) 0))

(defn line-start[t]
  (let [newpos (pos-line-start (t :pos) (t :str))]
    (buf-set-pos t newpos)))

(defn line-end[t]
  (let [newpos (first (pos-re+ (t :pos) (t :str) #"(?m)$"))]
    ;(println newpos)
    (buf-set-pos t newpos)))

;(line-end {:x 0 :y 0 :str (rope "aaa") :pos 0})

  ;(text-re t  pos-re+ (-> t :str count dec)))

