(ns webvim.text
  (:use clojure.pprint
        webvim.global
        (clojure [string :only (join split blank?)])))

(def line-break "\n")
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

(defn- re-forward
  [s pos re]
  (let [m (re-matcher re s)]
    (find-first m pos)))

(defn- re-backward
  [s pos re]
  (let [m (re-matcher (re-pattern (str "(?=" re ")")) s)
        m1 (re-matcher re s)]
    (.useTransparentBounds m true)
    (.useTransparentBounds m1 true)
    (loop [offset pos]
      (println "offset:" offset)
      (if (neg? offset)
        nil
        (let[[a b] (if (< offset 1)
                     [0 offset]
                     [(- offset 1) offset])]
          (.region m a b)
          (let [matches (find-last m)]
            (println matches)
            (println a b)
            (if (nil? matches)
              (recur (dec a))
              (find-first m1 (first matches)))))))))

(defn- re-next-forward [s pos re]
  (re-forward s (inc pos) re))

(defn- re-next-backward [s pos re]
  (re-backward s (dec pos) re))

(defn text-subs
  ([s l r]
   (subs s l r))
  ([s l]
   (subs s l)))

(defn count-lines[s]
  (let [cnt (count line-break)]
    (loop[s1 s n 0]
      (let [i (.indexOf s1 line-break)]
        (if (= i -1)
          n
          (recur (text-subs s1 (+ i cnt)) (inc n)))))))

(defn text-offset-pos
  [t offset]
  (if (zero? offset) t
    (let [pos (t :pos)
          s (t :str)]
      (if (pos? offset)
        (let[len (count s)
             l pos
             tmpr (+ l offset)
             r (if (> tmpr len)
                 len
                 tmpr)
             sub (text-subs s l r)
             lastEOL (re-next-backward (t :str) r re-line-break)]
          (-> t
              (assoc :pos r)
              (assoc :x (if (nil? lastEOL) r (- r (last lastEOL))))
              (assoc :y (+ (t :y) (count-lines sub)))))
        (let[r pos
             tmpl (+ r offset)
             l (if (neg? tmpl) 0 tmpl)
             sub (text-subs s l r)
             lastEOL (re-next-backward (t :str) l re-line-break)]
          (-> t
              (assoc :pos l)
              (assoc :x (if (nil? lastEOL) l (- l (last lastEOL))))
              (assoc :y (- (t :y) (count-lines sub)))))))))

(defn text-size
  "How many chars and lines s contains"
  [s]
  {:dpos (count s)
   :dy (count-lines s)})

(defn text-op-size
  [t op {dpos :dpos dy :dy}]
  (-> t
      (update-in [:pos] op dpos)
      (update-in [:y] op dy)))

(defn text-x
  "Update :x"
  [t]
  (let [pos (t :pos)
        lastEOL (re-next-backward (t :str) pos re-line-break)]
    (assoc t :x (if (nil? lastEOL) pos (- pos (last lastEOL))))))

(defn text-update-pos[t newpos]
  (let [pos (t :pos)
        s (t :str)]
    (cond 
      (> newpos pos)
      (-> t
          (text-op-size + (text-size (text-subs s pos newpos)))
          text-x)
      (< newpos pos)
      (-> t
          (text-op-size - (text-size (text-subs s newpos pos)))
          text-x)
      :else t)))

(defn text-replace [t l from to]
  (let [s (t :str)
        pos (t :pos)
        r (+ l (count from))
        prefix (text-subs s 0 l)
        suffix (text-subs s r)
        news (str prefix to suffix)
        newt (assoc t :str news)]
    (cond 
      (< pos l)
      newt
      (>= pos r)
      (-> newt
          (text-op-size - (text-size from))
          (text-op-size + (text-size to))
          text-x)
      :else
      (-> newt
          (text-op-size - (text-size (text-subs s l pos)))
          (text-op-size + (text-size to))
          text-x))))

(defn text-char-at
  "return nil if out of range"
  [s pos]
  (if (or (neg? pos) (>= pos (count s)))
    nil
    (.charAt s pos)))

(defn char-backward [t]
  (let [pos (t :pos)
        s (t :str)
        newpos (dec pos)
        ch (text-char-at s newpos)]
    (if (or (nil? ch) (= ch \newline)) t
      (-> t
          (assoc :pos newpos)
          (update-in [:x] dec))))) 

(defn char-forward [t]
  (let [pos (t :pos)
        s (t :str)
        newpos (inc pos)
        ch (text-char-at s newpos)]
    (if (or (nil? ch) (= ch \newline)) t
      (-> t
          (assoc :pos newpos)
          (update-in [:x] inc))))) 

(defn eol-backward[s pos]
  (or (first (re-next-backward s pos re-line-break)) -1))

(defn eol-forward[s pos]
  (or (first (re-forward s pos re-line-break)) (count s)))

(defn line-backward [t vx]
  (let [{pos :pos
         s :str
         y :y} t]
    (let [eol (eol-backward s pos)]
      (if (neg? eol) t
        (let [eol2 (eol-backward s eol)
              len (- eol eol2)
              x (bound-range vx 0 (dec len))]
          (-> t
            (assoc :pos (+ eol2 x 1))
            (assoc :x x)
            (assoc :y (dec y))))))))

(defn line-forward [t vx]
  (let [{pos :pos
         s :str
         y :y} t]
    (let [eol (eol-forward s pos)]
      (if (= eol (count s)) t
        (let [eol2 (eol-forward s (inc eol))
              len (- eol2 eol)
              x (bound-range vx 0 (dec len))]
          (-> t
            (assoc :pos (+ eol x 1))
            (assoc :x x)
            (assoc :y (inc y))))))))

(defn lines-forward[t n]
  (if (zero? n) t
    (recur (line-forward t (t :x)) (dec n))))

(defn lines-backward[t n]
  (if (zero? n) t
    (recur (line-backward t (t :x)) (dec n))))

(defn text-start[t]
  (merge t {:x 0 :y 0 :pos 0}))

(defn move-re[t re re-fn]
  (let [{pos :pos
         s :str
         y :y} t
        newpos (or (first (re-fn s pos re)) pos)]
    (println "newpos:" newpos)
    (text-update-pos t newpos)))

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

(defn word-forward[t]
  (println "word-forward")
  (move-re t re-word-start-border re-next-forward))

(defn word-backward[t]
  (move-re t re-word-start-border re-next-backward))

(defn WORD-forward[t]
  (move-re t re-WORD-start-border re-next-forward))

(defn WORD-backward[t]
  (move-re t re-WORD-start-border re-next-backward))

(defn word-end-forward[t]
  (move-re t re-word-end-border re-next-forward))

(defn WORD-end-forward[t]
  (move-re t re-WORD-end-border re-next-forward))

(defn line-first[t]
  (let [s (t :str)
        pos (t :pos)
        newpos (inc (eol-backward s pos))]
    (text-update-pos t newpos)))

;(def re-line-start #"^\s*?\S|\n\s*?\S")

(defn line-start[t]
  (-> t
      line-first
      (move-re #"(?<=\s)[\S\n]" re-forward)))

(defn line-end[t]
  (move-re t #"\n|$" re-forward))
