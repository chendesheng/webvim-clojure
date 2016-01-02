;first half part is about "one line" second half part is about "lines"
(ns webvim.core.line
  (:use clojure.pprint
        webvim.core.rope
        webvim.core.utils
        webvim.core.pos))

(defn pos-line-first[r pos]
  (or (first (pos-re- r pos #"(?m)^")) 0))
;(pos-line-first (rope "aa\naaa") 4)

(defn pos-line-last[r pos]
  (or (first (pos-re+ r (inc pos) #"(?m)^")) (count r)))

(defn pos-line[r pos]
  (let [a (pos-line-first r pos)
        b (pos-line-last r pos)]
    [a b]))

;(pos-line (rope "aa\nbb") 1)

(defn pos-line-start[r pos]
  (let [lf (pos-line-first r pos)]
    (or (first (pos-re+ r lf #"[\S\n]|((?<=\s)[\S\n])")) 0)))

(defn pos-line-end[r pos]
  (first (pos-re+ r pos #"(?m)$")))


(defn line-first[buf]
  (buf-move buf pos-line-first))

(defn line-last[buf]
  (buf-move buf pos-line-last))

(defn line-start[buf]
  (buf-move buf pos-line-start))

(defn line-end[buf]
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

(defn- lines-move[buf n fndir]
  (let [vx (buf :x)]
    (buf-move buf
              (fn [r pos]
                (let [rg (nth (fndir r pos) n nil)]
                  (if (nil? rg) pos
                    (let [[a b] rg
                          s (subr r a b)
                          cx (visualx-to-charx s vx (buf :tabsize))]  
                      (+ a (bound-range cx 0 (- b a 1))))))))))

(defn lines-n+[buf n]
  (lines-move buf n pos-lines-seq+))

;(lines-n+ {:pos 0 :x 2 :str (rope "abc\n\n") :y 0} 1)

(defn lines-n-[buf n]
  (lines-move buf n pos-lines-seq-))

(defn lines-row[buf n]
  (let [y (buf :y)
        dy (- n y)]
    (if (pos? dy)
      (lines-n+ buf dy)
      (lines-n- buf (- dy)))))

(defn make-linewise-range [[a b] buf]
  (println "make-linewise-range:" a b)
  (let [r (buf :str)
        [a b] (sort2 a b)]
    [(pos-line-first r a) (pos-line-last r b)]))

;FIXME: handle /tab
(defn expand-block-ranges[r a b]
  (println "expand-block-ranges" a b)
  (let [[a b] (sort2 a b)
        [ca cb] (sort2 (- a (pos-line-first r a)) ;column a, b
                       (- b (pos-line-first r b)))
        lines (filter
                (fn[[a b]] (> (- b a) ca))
                (map (fn[[a b]]
                       [a (- b 2)]) (pos-lines-seq+ r a b)))]
    (println lines)
    (vec (map (fn[[a b]]
                [(+ a (max ca 0))
                 (+ a (min cb (- b a)))])
              lines))))

(defn test-expand[]
  (expand-block-ranges
    (rope "hello\na\nhello") 0 9))

(defn first-line[r]
  (subr r (-> r pos-lines-seq+ first)))
