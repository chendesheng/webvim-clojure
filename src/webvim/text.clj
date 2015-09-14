(ns webvim.text
  (:use clojure.pprint
        webvim.change
        webvim.global)
  (:import (org.ahmadsoft.ropes RopeBuilder)))

(defn text-subs-range[s [a b]]
  (text-subs s a b))

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

(defn count-left-spaces[s]
  (count (re-subs #"^\s*" s)))

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

(defn pos-re-next-forward 
  "return forward range matches, exclude current position"
  [pos s re]
  (pos-re-forward (inc pos) s re))

(defn pos-re-next-backward 
  "return backward range matches, exclude current position"
  [pos s re]
  (pos-re-backward (dec pos) s re))

;function name start with pos take pos as first argument and return newpos
;these can be combined in one motion command
(defn pos-re
  ([pos s re re-fn notfound]
   (or (first (re-fn pos s re)) notfound))
  ([pos s re re-fn]
   (pos-re pos s re re-fn pos)))

(defn- pos-end-re[pos s re re-fn]
  (or (last (re-fn pos s re)) pos))

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

(defn text-start[t]
  (merge t {:x 0 :y 0 :pos 0}))

(defn text-re
  ([t re re-fn not-found]
   (let [{pos :pos
          s :str} t
         newpos (pos-re pos s re re-fn not-found)]
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
  (text-re t re pos-re-next-forward (-> t :str count dec))))

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
  (re-backward t #"((?<=\n)\n[^\n])"))

;(paragraph-forward {:str (text-new "aaa\nbb") :pos 0 :y 0})

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
         s :str} t]
    (pos-word pos s)))

(defn pos-word[pos s]
  (let [b (last (pos-re-forward pos s re-word-end-border))
        a (first (pos-re-backward b s re-word-start-border))]
      [a b]))

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
;(pos-re-forward 0 (text-new "   ()") #"\(|\)|\[|\]|\{|\}")

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
