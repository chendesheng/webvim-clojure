;first half part is about "one line" second half part is about "lines"
(ns webvim.core.line
  (:require [webvim.core.lineindex :refer [range-by-line range-by-pos total-length
                                           pos-linenum total-lines]])
  (:use clojure.pprint
        webvim.core.rope
        webvim.core.utils
        webvim.core.pos))

(defn pos-line
  ([{lidx :lineindex} pos]
    (range-by-pos lidx pos))
  ([{lidx :lineindex pos :pos}]
    (range-by-pos lidx pos)))

(defn pos-line-first
  ([{lidx :lineindex} pos] 
    (first (range-by-pos lidx pos)))
  ([{pos :pos lidx :lineindex}]
    (first (range-by-pos lidx pos))))
;(pos-line-first (rope "aa\naaa") 4)

(defn pos-line-last
  ([{lidx :lineindex} pos] 
    (last (range-by-pos lidx pos)))
  ([{pos :pos lidx :lineindex}]
    (last (range-by-pos lidx pos))))

;(pos-line (rope "aa\nbb") 1)

(defn pos-line-start 
  ([buf pos]
    (let [lf (pos-line-first buf pos)]
      (or (first (pos-re+ (buf :str) lf #"[\S\n]|((?<=\s)[\S\n])")) 0)))
  ([{pos :pos :as buf}]
    (pos-line-start buf pos)))

(defn pos-line-end
  ([{r :str} pos]
    (first (pos-re+ r pos #"(?m)$")))
  ([buf]
    (pos-line-end buf (buf :pos))))

(defn buf-move-line 
  [buf fnmove]
  (buf-set-pos buf
               (or (fnmove buf) (buf :pos))))

(defn line-first [buf]
  (buf-move-line buf pos-line-first))

(defn line-last [buf]
  (buf-move-line buf pos-line-last))

(defn line-start [buf]
  (buf-move-line buf pos-line-start))

(defn line-end [buf]
  (buf-move-line buf pos-line-end))

;(line-end {:x 0 :y 0 :str (rope "aaa") :pos 0})

(defn pos-lines-seq-
  "return a lazy seq, line by line start at pos back to top"
  ([buf pos]
    (let [rg (pos-line buf pos)
          [a _] rg]
      (if (pos? a)
        (cons rg (lazy-seq (pos-lines-seq- buf (dec a))))
        (list rg))))
  ([{pos :pos :as buf}]
    (pos-lines-seq- buf pos)))

(defn pos-lines-seq+
  "return a lazy seq, line by line start at pos until bottom"
  ([{lidx :lineindex :as buf} pos]
    (let [[_ b :as rg] (pos-line buf pos)]
      (if (< b (total-length lidx))
        (cons rg (lazy-seq (pos-lines-seq+ buf b)))
        (list rg))))
  ([buf] (pos-lines-seq+ buf (buf :pos)))
  ([buf a b] ;exclusive
    (take-while
      #(-> % first (< b))
      (pos-lines-seq+ buf a))))

;(pos-lines-seq+ (rope "aa\nbb\ncc\n\n") 0 1)

(defn lines-n [{lidx :lineindex pos :pos :as buf} delta]
  (let [n (+ (pos-linenum lidx pos) delta)]
    (if (or (< n 0)
            (>= n (total-lines lidx)))
      buf
      (buf-move-line buf
                     (fn [{r :str :as buf}]
                       (let [[a b] (range-by-line lidx n)
                             s (subr r a b)
                             cx (visualx-to-charx s (buf :x) (buf :tabsize))]  
                         (+ a (bound-range cx 0 (-> s count dec)))))))))

(defn lines-row [buf n]
  (buf-move-line buf
                 (fn [{lidx :lineindex :as buf}]
                   (first (range-by-line lidx n)))))

(defn make-linewise-range [[a b] buf]
  ;(println "make-linewise-range:" a b)
  (let [[a b] (sort2 a b)]
    [(pos-line-first buf a) (pos-line-last buf b)]))

;get vertical line start at pos up/down h lines
(defn vertical-line-pos [buf pos h tabsize skip-hole?]
  (let [r (buf :str)
        lines (if (pos? h)
                (take h (pos-lines-seq+ buf pos))
                (take (- h) (pos-lines-seq- buf pos)))
        a (pos-line-first buf pos)
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
  ([buf a b tabsize]
     ;(println "expand-block-ranges" a b)
    (let [h (inc (-> buf :str (subr (sort2 a b)) count-<br>))]
       ;(println h a b)
      (if (< a b)
        (map sort-column  ;zip
             (vertical-line-pos buf a h tabsize false)
             (reverse (vertical-line-pos buf b (- h) tabsize false))
             (take h (map second (pos-lines-seq+ buf a))))
        (map sort-column
             (vertical-line-pos buf b h tabsize false)
             (reverse (vertical-line-pos buf a (- h) tabsize false))
             (take h (map second (pos-lines-seq+ buf b)))))))
  ([buf [a b] tabsize]
    (expand-block-ranges buf a b tabsize)))

(defn test-expand []
  (vec (expand-block-ranges
         (rope "he\tllo") 3 4 4)))
;he  llo
;a
;hello

(defn first-line [{r :str lidx :lineindex}]
  (subr r (range-by-line lidx 0)))

(defn move-to-line [buf row]
  (-> buf
      (lines-row row)
      line-start))

(defn column [buf]
  (dec (visual-size 
         (subr (buf :str)
               (pos-line-first buf)
               (-> buf :pos inc)) 
         (buf :tabsize))))

(defn buf-update-column [buf]
  (assoc buf :x (column buf)))

(defn line-str
  ([{r :str :as buf} pos]
    (subr r (pos-line buf pos)))
  ([buf]
    (line-str buf (buf :pos))))

(defn line-range[{lidx :lineindex} linenum]
  (range-by-line lidx linenum))
