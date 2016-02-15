;first half part is about "one line" second half part is about "lines"
(ns webvim.core.line
  (:use clojure.pprint
        webvim.core.rope
        webvim.core.utils
        webvim.core.pos))

(defn pos-line-first [r pos]
  (or (first (pos-re- r pos #"(?m)^")) 0))
;(pos-line-first (rope "aa\naaa") 4)

(defn pos-line-last [r pos]
  (or (first (pos-re+ r (inc pos) #"(?m)^")) (count r)))

(defn pos-line [r pos]
  [(pos-line-first r pos)
   (pos-line-last r pos)])

;(pos-line (rope "aa\nbb") 1)

(defn pos-line-start [r pos]
  (let [lf (pos-line-first r pos)]
    (or (first (pos-re+ r lf #"[\S\n]|((?<=\s)[\S\n])")) 0)))

(defn pos-line-end [r pos]
  (first (pos-re+ r pos #"(?m)$")))


(defn line-first [buf]
  (buf-move buf pos-line-first))

(defn line-last [buf]
  (buf-move buf pos-line-last))

(defn line-start [buf]
  (buf-move buf pos-line-start))

(defn line-end [buf]
  (buf-move buf pos-line-end))

;(line-end {:x 0 :y 0 :str (rope "aaa") :pos 0})

(defn pos-lines-seq-
  "return a lazy seq, line by line start at pos back to top"
  [r pos]
  (let [rg (pos-line r pos)
        [a _] rg]
    (if (pos? a)
      (cons rg (lazy-seq (pos-lines-seq- r (dec a))))
      (list rg))))

(defn pos-lines-seq+
  "return a lazy seq, line by line start at pos until bottom"
  ([r pos]
    (let [rg (pos-line r pos)
          [_ b] rg]
      (if (< b (count r))
        (cons rg (lazy-seq (pos-lines-seq+ r b)))
        (list rg))))
  ([r] (pos-lines-seq+ r 0))
  ([r a b] ;both inclusive
    (take-while
      #(>= b (first %))
      (pos-lines-seq+ r a))))

;(pos-lines-seq+ (rope "aa\nbb\ncc\n\n") 0 0)

(defn- lines-move [buf n fndir]
  (let [vx (buf :x)]
    (buf-move buf
              (fn [r pos]
                (let [rg (nth (fndir r pos) n nil)]
                  (if (nil? rg) pos
                      (let [[a b] rg
                            s (subr r a b)
                            cx (visualx-to-charx s vx (buf :tabsize))]  
                        (+ a (bound-range cx 0 (- b a 1))))))))))

(defn lines-n [buf n]
  (cond 
    (pos? n) (lines-move buf n pos-lines-seq+)
    (neg? n) (lines-move buf (- n) pos-lines-seq-)
    :else buf))

(defn lines-row [buf n]
  (let [y (buf :y)
        dy (- n y)]
    (lines-n buf dy)))

(defn make-linewise-range [[a b] buf]
  ;(println "make-linewise-range:" a b)
  (let [r (buf :str)
        [a b] (sort2 a b)]
    [(pos-line-first r a) (pos-line-last r b)]))

;get vertical line start at pos up/down h lines
(defn vertical-line-pos [r pos h tabsize skip-hole?]
  (let [lines (if (pos? h)
                (take h (pos-lines-seq+ r pos))
                (take (- h) (pos-lines-seq- r pos)))
        a (pos-line-first r pos)
        vx (visual-size (str (subr r a pos)) tabsize)]
    ;(println pos a vx)
    (map (fn [[a b]]
           (let [s (str (subr r a b))
                 x (+ (visualx-to-charx s vx tabsize) a)]
             (if skip-hole? 
               (if (>= x b) -1 x) ;use -1 fill "hole"
               x))) lines)))

;(defn test-vertical-line-pos[]
;(vec (vertical-line-pos (rope "aaaaa\n\tbb\nx\ty") 12 -3 4)))
;aaaaa
;    bb
;x   y

(defn- sort-column [a b eol]
  (let [[a b] (sort2 a b)]
    [(if (= a eol) (dec a) a) 
     (if (= b eol)
       (- b 2) b)])) ;if b < a select empty; both sides inclusive; length=b-a+1

(defn expand-block-ranges
  ([r a b tabsize]
     ;(println "expand-block-ranges" a b)
    (let [h (inc (-> r (subr (sort2 a b)) count-<br>))]
       ;(println h a b)
      (if (< a b)
        (map sort-column  ;zip
             (vertical-line-pos r a h tabsize false)
             (reverse (vertical-line-pos r b (- h) tabsize false))
             (take h (map second (pos-lines-seq+ r a))))
        (map sort-column
             (vertical-line-pos r b h tabsize false)
             (reverse (vertical-line-pos r a (- h) tabsize false))
             (take h (map second (pos-lines-seq+ r b)))))))
  ([r [a b] tabsize]
    (expand-block-ranges r a b tabsize)))

(defn test-expand []
  (vec (expand-block-ranges
         (rope "he\tllo") 3 4 4)))
;he  llo
;a
;hello

(defn first-line [r]
  (subr r (-> r pos-lines-seq+ first)))
