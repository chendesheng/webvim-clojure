(ns webvim.text
  (:use clojure.pprint
        webvim.change
        webvim.global)
  (:import (org.ahmadsoft.ropes RopeBuilder)))

(defn re-test[re s]
  (cond (nil? re) false
        (string? s) (.find (re-matcher re s))
        :else (.find (.matcher s re))))

(defn re-subs[re s]
  (cond (nil? re) nil
        (string? s) (re-find re s)
        :else (let [m (.matcher s re)]
                (if (.find m)
                  (subr s (.start m) (.end m))
                  nil))))

(defn count-left-spaces[s]
  (count (re-subs #"^\s*" s)))

(defn- find-first
  ([m pos]
   (if (.find m pos) ;!! (.find m pos) will reset m (m.region m.useTransparetBounds etc.) first
     [(.start m) (.end m)]
     nil))
  ([m]
   (if (.find m)
     [(.start m) (.end m)]
     nil)))

(defn- find-last[m]
  (if (.find m)
    (loop [m1 m]
      (let [matched [(.start m1) (.end m1)]]
        (if (.find m1)
          (recur m1)
          matched)))
    nil))

(defn rblank? 
  ([s]
   (or (nil? s)
       (let [m (.matcher s #"^\s$")]
         (.find m 0))))
  ([s rg]
   (rblank? (subr s rg))))

(defn pos-re-forward
  "return forward range matches"
  [pos s re]
  (let [m (.matcher s re)]
    (find-first m pos)))


(defn pos-re-backward
  "return backward range matches"
  [pos s re]
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
        (let[a (max 0 (- offset 50))
              b offset]
          (.region m a b)
          (let [matches (find-last m)]
            ;(println matches)
            ;(println a b)
            (if (nil? matches)
              (recur (dec a))
              (find-first m1 (first matches)))))))))

;(pos-re-backward 2 (rope "aa bb") #"(?<=[^a-zA-Z_\-!.?+*=<>&#\':0-9])[a-zA-Z_\-!.?+*=<>&#\':0-9]|(?<=[a-zA-Z_\-!.?+*=<>&#\':0-9\s,])[^a-zA-Z_\-!.?+*=<>&#\':0-9\s,]")

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

(defn buf-insert
  ([t l txt]
   (buf-replace t l l txt))
  ([t txt]
   (buf-insert t (t :pos) txt)))

(defn buf-delete
  ([t a b]
   (buf-replace t a b ""))
  ([t b]
   (let [pos (t :pos)
         [a b] (sort2 pos b)]
     (buf-delete t a b))))

(defn text-delete-range
  "delete range and set pos to end of deleted"
  [t rg]
  (-> t
      (buf-delete (first rg) (second rg))
      (buf-set-pos (first rg))))

(defn text-delete-offset[t offset] 
  (let [pos (t :pos)
        newpos (+ pos offset)]
    (if (neg? newpos) t
      (buf-delete t newpos))))

(defn char-at
  "return nil if out of range"
  [s pos]
  (if (or (neg? pos) (>= pos (count s)))
    nil
    (.charAt s pos)))

(defn text-start[t]
  (merge t {:x 0 :y 0 :pos 0}))

(defn text-end[t]
  (-> t
      (assoc :y (-> t :linescnt dec))
      (assoc :pos (-> t :str count dec))))

(defn text-re
  ([t re re-fn not-found]
   (let [{pos :pos
          s :str} t
         newpos (pos-re pos s re re-fn not-found)]
     (buf-set-pos t newpos)))
  ([t re re-fn]
   (text-re t re re-fn 0)))

;(defn buf-move 
;  [buf fnmove]
;  (let [{pos :pos
;         r :str} buf
;        newpos (fnmove pos r)]
;    (buf-set-pos buf newpos)))

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

;(paragraph-forward {:str (rope "aaa\nbb") :pos 0 :y 0})

(defn add-highlight[t rg]
  (let [highlights (t :highlights)]
    (if (empty? (filter (fn[[a b]]
                          (and (= a (rg 0)) (= b (rg 1)))) highlights))
      (update-in t [:highlights] conj rg) t)))

(defn re-forward-highlight[t re]
  (let [pos (t :pos)
        s (t :str)
        rg (or 
             (pos-re-next-forward pos s re)
             (pos-re-forward 0 s re))] ;TODO: use region reduce duplicate work
    (if (nil? rg) t
      (let [[a b] rg]
        (-> t
            (buf-set-pos a)
            (add-highlight [a (dec b)]))))))

(defn re-backward-highlight[t re]
  (let [pos (t :pos)
        s (t :str)
        rg (or 
             (pos-re-next-backward pos s re)
             (pos-re-next-backward (count s) s re))]
    (if (nil? rg) t
      (let [[a b] rg]
        (-> t
            (buf-set-pos a)
            (add-highlight [a (dec b)]))))))

(defn char-forward[t]
  (let [pos (t :pos)
        newpos (inc pos)
        s (t :str)
        ch (char-at s pos)]
    (if (= (or ch \newline) \newline)
      t
      (buf-set-pos t newpos))))

(defn char-backward[t]
  (let [newpos (dec (t :pos))
        s (t :str)
        ch (char-at s newpos)]
    (if (= (or ch \newline) \newline)
      t
      (buf-set-pos t newpos))))

(defn pos-word[pos s]
  (let [re-start (re-pattern (str "([" not-word-chars "](?=[" word-chars "]))|((?<=[" not-word-chars "])$)"))
        re-end (re-pattern (str "[" word-chars "](?=[" not-word-chars "])"))
        ;_ (println s)
        ;_ (println re-start)
        ;_ (println re-end)
        b (or (last (pos-re-forward pos s re-end)) (count s))
        a (or (last (pos-re-next-backward b s re-start)) 0)]
      [a b]))

;(pos-word 2 (rope "aaa"))

(defn current-word[t]
  "return range of word under cursor, right side is exclusive"
  (let [{pos :pos
         s :str} t]
    ;(println pos s)
    (pos-word pos s)))

;(pos-word 2 (rope "(ns w"))
;(pos-re-backward 3 (rope "(ns w") #"[^a-zA-Z_\-!.?+*=<>&#\':0-9](?=[a-zA-Z_\-!.?+*=<>&#\':0-9])")

;(defn positive-numbers 
;  ([] (positive-numbers 1))
;  ([n] 
;   (println n)
;   (cons n (lazy-seq (positive-numbers (inc n))))))
;(take 1 (positive-numbers))

(defn ranges-to-texts
  "Return a lazy seq contains texts sub from s. Range's right point is exclusive."
  [s ranges]
  (map #(subr s %) ranges))

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

;(pos-re-forward-seq -1 (rope "(((") #"\(")
;(pos-re-backward-seq -1 (rope "(((") #"\(")
;(pos-re-forward 0 (rope "   ()") #"\(|\)|\[|\]|\{|\}")

(defn pos-match-brace
  "return matched brace position, nil if not find"
  [s pos]
  (let [brace (char-at s pos)
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
                     (let [ch (char-at s a)
                           newcnt (if (inc-cnt? ch)
                                    (inc cnt)
                                    (dec cnt))]
                       (if (zero? newcnt)
                         (reduced [a])
                         newcnt))) 0 braces)]
        (if (vector? mpos) (first mpos) nil)))))

(defn highlight-all-matches[b re]
  (let [s (b :str)]
    (assoc b :highlights 
           (map (fn[[a b]]
                  [a (dec b)])
                (pos-re-forward-seq 0 s re)))))

