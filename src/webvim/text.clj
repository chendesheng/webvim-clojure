(ns webvim.text
  (:use clojure.pprint
        webvim.change
        webvim.global)
  (:import (org.ahmadsoft.ropes RopeBuilder)))

(defn text-subs-range[s [a b]]
  (text-subs s a b))

(defn text-update-pos[t newpos]
  (let [pos (t :pos)
        s (t :str)]
    (cond 
      (> newpos pos)
      (-> t
          (text-op-size + (text-size (text-subs s pos newpos))))
      (< newpos pos)
      (-> t
          (text-op-size - (text-size (text-subs s newpos pos))))
      :else t)))

(defn re-test[re s]
  (cond (nil? re) false
        (string? s) (.find (re-matcher re s))
        :else (.find (.matcher s re))))

(defn re-subs[re s]
  (cond (nil? re) nil
        (string? s) (re-find re s)
        :else (let [m (.matcher s re)]
                (if (.find m)
                  (text-subs s (.start m) (.end m))
                  nil))))

(defn count-left-spaces[line]
  (count (re-subs #"^\s*" line)))

(def re-line-break (re-pattern line-break))

(defn- find-first[m pos]
  (if (.find m pos)
    [(.start m) (.end m)]
    nil))

(defn- find-last[m]
  (if (.find m)
    (loop [m1 m]
      (let [matched [(.start m1) (.end m1)]]
        (if (.find m1)
          (recur m1)
          matched)))
    nil))

(defn text-blank? [s]
  (or (nil? s)
      (let [m (.matcher s #"^\s$")]
        (.find m 0))))

(defn pos-re-forward
  "return forward range matches"
  [pos s re]
  (let [m (.matcher s re)]
    (find-first m pos)))

(defn pos-re-backward
  "return backward range matches"
  [pos s re]
  ;(println s)
  ;(println re)
  (let [m (.matcher s (re-pattern (str "(?=" re ")")))
        m1 (.matcher s re)]
    (.useTransparentBounds m true)
    (.useTransparentBounds m1 true)
    (.useAnchoringBounds m false)
    (.useAnchoringBounds m1 false)
    (loop [offset pos]
      ;(println "offset:" offset)
      (if (neg? offset)
        nil
        (let[[a b] (if (< offset 100)
                     [0 offset]
                     [(- offset 100) offset])]
          (.region m a b)
          (let [matches (find-last m)]
            ;(println matches)
            ;(println a b)
            (if (nil? matches)
              (recur (dec a))
              (find-first m1 (first matches)))))))))

(defn- pos-re-next-forward 
  "return forward range matches, exclude current position"
  [pos s re]
  (pos-re-forward (inc pos) s re))

(defn- pos-re-next-backward 
  "return backward range matches, exclude current position"
  [pos s re]
  (pos-re-backward (dec pos) s re))

;function name start with pos take pos as first argument and return newpos
;these can be combined in one motion command
(defn- pos-eol-backward
  "return nil if coundn't find EOL"
  [pos s]
  (first (pos-re-next-backward pos s re-line-break)))

(defn- pos-eol-forward
  "return nil if coundn't find EOL"
  [pos s]
  (first (pos-re-forward pos s re-line-break)))

(defn- pos-end-eol-forward
  "return nil if coundn't find EOL"
  [pos s]
  (last (pos-re-forward pos s re-line-break)))

(defn- pos-end-eol-backward
  "return nil if coundn't find EOL"
  [pos s]
  (last (pos-re-backward pos s re-line-break)))

(defn- pos-re
  ([pos s re re-fn notfound]
   (or (first (re-fn pos s re)) notfound))
  ([pos s re re-fn]
   (pos-re pos s re re-fn pos)))

(defn- pos-end-re[pos s re re-fn]
  (or (last (re-fn pos s re)) pos))

(defn pos-line-first[pos s]
  (pos-re pos s #"((?<=\n)[\s\S])" pos-re-backward 0))

;(defn text-offset-pos
;  [t offset]
;  (if (zero? offset) t
;    (let [pos (t :pos)
;          s (t :str)]
;      (if (pos? offset)
;        (let[len (count s)
;             l pos
;             tmpr (+ l offset)
;             r (if (> tmpr len)
;                 len
;                 tmpr)
;             sub (text-subs s l r)
;             lastEOL (pos-re-backward r (t :str) re-line-break)]
;          (-> t
;              (assoc :pos r)
;              (assoc :x (if (nil? lastEOL) r (- r (last lastEOL))))
;              (assoc :y (+ (t :y) (count-lines sub)))))
;        (let[r pos
;             tmpl (+ r offset)
;             l (if (neg? tmpl) 0 tmpl)
;             sub (text-subs s l r)
;             lastEOL (pos-re-backward l (t :str) re-line-break)]
;          (-> t
;              (assoc :pos l)
;              (assoc :x (if (nil? lastEOL) l (- l (last lastEOL))))
;              (assoc :y (- (t :y) (count-lines sub)))))))))

(defn- text-save-change[t pos from to]
  (update-in t [:changes] conj {:pos pos :len (count from) :to (str to)}))

(defn text-insert
  ([t l txt]
   (text-replace t l l txt))
  ([t txt]
   (text-insert t (t :pos) txt)))

(defn text-delete
  ([t l r]
   (text-replace t l r ""))
  ([t pt]
   (let [pos (t :pos)
         [l r] (sort2 pos pt)]
     (text-delete t l r))))

(defn text-delete-inclusive
  [t p1 p2]
  (let [[l r] (sort2 p1 p2)]
    (-> t
        (text-delete l (inc r))
        (text-update-pos l))))

(defn text-delete-range
  "delete range and set pos to end of deleted"
  [t rg]
  (-> t
      (text-delete (first rg) (second rg))
      (text-update-pos (first rg))))

(defn text-insert-line-after[t]
  (let [pos (t :pos)
        s (t :str)
        newpos (pos-eol-forward pos s)]
    (if (nil? newpos)
      (-> t
          (text-insert (count s) line-break)
          (text-update-pos (-> s count dec)))
      (-> t
          (text-update-pos newpos)
          (text-insert line-break)))))

(defn text-insert-line-before[t]
  (let [pos (t :pos)
        s (t :str)
        newpos (pos-eol-backward pos s)]
    (if (nil? newpos)
      (-> t
          (text-insert 0 line-break)
          (text-update-pos 0))
      (-> t
          (text-update-pos newpos)
          (text-insert line-break)))))

(defn text-delete-offset[t offset] 
  (let [pos (t :pos)
        newpos (+ pos offset)]
    (if (neg? newpos) t
      (text-delete t newpos))))

(defn text-char-at
  "return nil if out of range"
  [s pos]
  (if (or (neg? pos) (>= pos (count s)))
    nil
    (.charAt s pos)))

(defn line-backward [t]
  (let [{pos :pos
         s :str
         x :x
         y :y} t]
    (let [eol (pos-eol-backward pos s)]
      (if (nil? eol) t
        (let [eol2 (or (pos-eol-backward eol s) -1)
              len (- eol eol2)
              x1 (bound-range x 0 (dec len))]
          (-> t
            (assoc :pos (+ eol2 x1 1))
            (assoc :y (dec y))))))))

(defn line-forward [t]
  (let [{pos :pos
         s :str
         x :x
         y :y} t]
    (let [eol (pos-eol-forward pos s)]
      (if (nil? eol) t
        (let [eol2 (or (pos-eol-forward (inc eol) s) (count s))
              len (- eol2 eol)
              x1 (bound-range x 0 (dec len))]
          (-> t
            (assoc :pos (+ eol x1 1))
            (assoc :y (inc y))))))))

(defn lines-forward[t n]
  (if (zero? n) t
    (recur (line-forward t) (dec n))))

(defn lines-backward[t n]
  (if (zero? n) t
    (recur (line-backward t) (dec n))))

(defn lines-row[t n]
  (let [y (t :y)
        dy (- n y)]
    (if (> dy 0)
      (lines-forward t dy)
      (lines-backward t (- dy)))))

(defn text-start[t]
  (merge t {:x 0 :y 0 :pos 0}))

(defn- text-re
  ([t re re-fn notfound]
   (let [{pos :pos
          s :str} t
         newpos (pos-re pos s re re-fn notfound)]
     (text-update-pos t newpos)))
  ([t re re-fn]
   (text-re t re re-fn 0)))

(def word-chars "a-zA-Z_\\-!.?+*=<>&#\\':0-9")
(def not-word-chars (str "^" word-chars))
(def space-chars "\\s,")
(def not-space-chars "^\\s,")
(def punctuation-chars (str "^" word-chars space-chars))
(def not-punctuation-chars (str word-chars space-chars))

(def re-word-start-border
  (re-pattern 
    (str "(?<=[" not-word-chars "])[" word-chars "]|(?<=[" not-punctuation-chars "])[" punctuation-chars "]")))

(def re-WORD-start-border
  (re-pattern 
    (str "(?<=[" space-chars "])[" not-space-chars "]")))

(def re-word-end-border
  (re-pattern 
    (str "[" word-chars "](?=[" not-word-chars "])|[" punctuation-chars "](?=[" not-punctuation-chars "])")))

(def re-WORD-end-border
  (re-pattern 
    (str "[" not-space-chars "](?=[" space-chars "])")))

(defn re-forward
  ([t re]
  (text-re t re pos-re-next-forward (-> t :str count))))

(defn re-backward[t re]
  (text-re t re pos-re-next-backward 0))

(defn word-forward[t]
  (re-forward t re-word-start-border))

(defn word-backward[t]
  (re-backward t re-word-start-border))

(defn WORD-forward[t]
  (re-forward t re-WORD-start-border))

(defn WORD-backward[t]
  (re-backward t re-WORD-start-border))

(defn word-end-forward[t]
  (re-forward t re-word-end-border))

(defn WORD-end-forward[t]
  (re-forward t re-WORD-end-border))

(defn paragraph-forward[t]
  (re-forward t #"(?<=\n)\n[^\n]"))

(defn paragraph-backward[t]
  (re-backward t #"((?<=\n)\n[^\n])|^"))

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
  (text-re t #"\n" pos-re-forward (-> t :str count)))

(defn re-forward-highlight[t re]
  (let [pos (t :pos)
        s (t :str)
        [a b] (pos-re-next-forward pos s re)]
    (-> t
        (text-update-pos a)
        (update-in [:highlights] conj a (dec b)))))

(defn re-backward-highlight[t re]
  (let [pos (t :pos)
        s (t :str)
        [a b] (pos-re-next-backward pos s re)]
    (-> t
        (text-update-pos a)
        (update-in [:highlights] conj a (dec b)))))

(defn char-forward[t]
  (let [pos (t :pos)
        newpos (inc pos)
        s (t :str)
        ch (text-char-at s pos)]
    (if (= (or ch \newline) \newline)
      t
      (text-update-pos t newpos))))

(defn char-backward[t]
  (let [newpos (dec (t :pos))
        s (t :str)
        ch (text-char-at s newpos)]
    (if (= (or ch \newline) \newline)
      t
      (text-update-pos t newpos))))

(defn current-word[t]
  "return range of word under cursor, right side is exclusive"
  (let [{pos :pos
         s :str} t
        b (last (pos-re-forward pos s re-word-end-border))
        a (first (pos-re-backward b s re-word-start-border))]
    [a b]))

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
;(pos-re-forward 0 (text-new "\n\naaa\n") #"\n")
;(pos-eol-forward 4 (text-new "\naaa\n"))
;(pos-eol-backward 4 (text-new "\naaa\n"))
;(pos-re-forward 0 (text-new "\n\naaa\n") #"(?<=\n)[\s\S]")

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
  ([s p1 p2] ;both inclusive
   (take-while
     #(>= p2 (first %))
     (lines s p1))))

;(lines (text-new "aa\nbb\ncc\n\n") 0 0)

;(let [s (text-new "aa\nbb\ncc\n\n")] 
;  (take 30
;        (lines s 0 9)))
;(lines (text-new "aa\nbb\ncc\n\n") 0)

;(defn positive-numbers 
;  ([] (positive-numbers 1))
;  ([n] 
;   (println n)
;   (cons n (lazy-seq (positive-numbers (inc n))))))
;(take 1 (positive-numbers))

(defn ranges-to-texts
  "Return a lazy seq contains texts sub from s. Range's right point is exclusive."
  [s ranges]
  (map #(apply text-subs s %) ranges))

(defn pos-next-line
"Find first line pred is true start at next line"
  ([s pos pred]
   (first 
     (filter pred (rest (lines s pos)))))
  ([s pos]
   (second (lines s pos))))

(defn pos-prev-line
  [s pos]
  (second (lines-reverse s pos)))

(defn pos-first-line
  "Find first line pred is true start at current line"
  [s pos pred]
  (first 
    (filter pred (lines s pos))))

(defn range-blank? [s rg]
  (text-blank? (text-subs-range s rg)))

(defn pos-re-forward-seq[pos s re]
  (if (neg? pos) nil
    (let [rg (pos-re-forward pos s re)]
      (if (nil? rg) nil
        (cons rg (lazy-seq (pos-re-forward-seq (-> rg first inc) s re)))))))

(defn pos-re-backward-seq[pos s re]
  (if (neg? pos) nil
    (let [rg (pos-re-backward pos s re)]
      (if (nil? rg) nil
        (cons rg (lazy-seq (pos-re-backward-seq (-> rg first dec) s re)))))))

;(pos-re-forward-seq -1 (text-new "(((") #"\(")
;(pos-re-backward-seq -1 (text-new "(((") #"\(")
(pos-re-forward 0 (text-new "   ()") #"\(|\)|\[|\]|\{|\}")

(defn pos-match-brace
  "return matched brace position, nil if not find"
  [s pos]
  (let [brace (text-char-at s pos)
        m (all-braces brace)
        left? (contains? left-braces brace)
        re (re-pattern (str  (quote-pattern brace) "|" (quote-pattern m)))]
    (if (nil? m) nil
      (let [inc-cnt? (if left? 
                       #(contains? left-braces %)
                       #(contains? right-braces %))
            braces (if left?
                     (pos-re-forward-seq pos s re)
                     (pos-re-backward-seq pos s re))
            mpos (reduce 
                   (fn[cnt [a _]]
                     (let [ch (text-char-at s a)
                           newcnt (if (inc-cnt? ch)
                                    (inc cnt)
                                    (dec cnt))]
                       (if (zero? newcnt)
                         (reduced [a])
                         newcnt))) 0 braces)]
        (if (vector? mpos) (first mpos) nil)))))

