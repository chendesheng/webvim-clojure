(ns webvim.core.line
  (:use clojure.pprint
        webvim.core.rope
        webvim.core.pos))

(defn pos-line[r pos]
  (let [a (first (pos-re- r pos #"(?m)^"))
        b (or (first (pos-re+ r (inc pos) #"(?m)^")) (count r))]
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

(defn- lines-move[buf n fndir]
  (let [x (buf :x)]
    (buf-move buf
              (fn [r pos]
                (let [rg (nth (fndir r pos) n nil)]
                  (if (nil? rg) pos
                    (let [[a b] rg]  
                      (+ a (bound-range x 0 (- b a 1))))))))))

(defn lines-forward[buf n]
  (lines-move buf n lines))

;(lines-forward {:pos 0 :x 2 :str (rope "abc\n\n") :y 0} 1)

(defn lines-backward[buf n]
  (lines-move buf n -lines))

(defn lines-row[buf n]
  (let [y (buf :y)
        dy (- n y)]
    (if (pos? dy)
      (lines-forward buf dy)
      (lines-backward buf (- dy)))))

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
  [buf]
   "return range of line under cursor, right side is exclusive"
   (pos-line (buf :str) (buf :pos)))

;(current-line {:pos 1 :str (rope "\naaa\n")})
;(current-line {:pos 5 :str (rope "\nbb\n\naaa\n")})

(defn pos-line-first[r pos]
  (or (first (pos-re- r pos #"(?m)^")) 0))

;(pos-line-first (rope "aa\naaa") 4)

(defn line-first[buf]
  (buf-move buf pos-line-first))

(defn pos-line-start[r pos]
  (let [lf (pos-line-first r pos)]
    (or (first (pos-re+ r lf #"[\S\n]|((?<=\s)[\S\n])")) 0)))

(defn line-start[buf]
  (buf-move buf pos-line-start))

(defn line-end[buf]
  (buf-move buf #(first (pos-re+ %1 %2 #"(?m)$")))) 

;(line-end {:x 0 :y 0 :str (rope "aaa") :pos 0})

  ;(text-re buf  pos-re+ (-> buf :str count dec)))

