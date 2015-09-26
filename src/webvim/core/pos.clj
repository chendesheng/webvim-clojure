(ns webvim.core.pos
  (:use webvim.core.rope))

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

(defn pos-re+
  "return forward range matches"
  [pos r re]
  (let [m (.matcher r re)]
    (find-first m pos)))

(defn pos-re-
  "return backward range matches"
  [pos r re]
  (let [m (.matcher r (re-pattern (str "(?=" re ")")))
        m1 (.matcher r re)]
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

(defn buf-move 
  [buf fnmove]
  (let [{pos :pos
         r :str} buf
        newpos (fnmove r pos)]
      (buf-set-pos buf newpos)))

(defn buf-start[buf]
  (merge buf {:x 0 :y 0 :pos 0}))

(defn buf-end[buf]
  (-> buf
      (assoc :y (-> buf :linescnt dec))
      (assoc :pos (-> buf :str count dec))))

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

(defn re-forward [buf re]
  (buf-move buf 
            (fn[r pos]
              (or (first (pos-re+ (inc pos) r re)) 
                  (-> r count dec)))))

(defn re-backward[buf re]
  (buf-move buf 
            (fn[r pos]
              (or (first (pos-re- (dec pos) r re)) 0))))

(defn word-forward[buf]
  (re-forward buf re-word-start-border))

(defn word-backward[buf]
  (re-backward buf re-word-start-border))

(defn WORD-forward[buf]
  (re-forward buf re-WORD-start-border))

(defn WORD-backward[buf]
  (re-backward buf re-WORD-start-border))

(defn word-end-forward[buf]
  (re-forward buf re-word-end-border))

(defn WORD-end-forward[buf]
  (re-forward buf re-WORD-end-border))

(defn paragraph-forward[buf]
  (re-forward buf #"(?<=\n)\n[^\n]"))

(defn paragraph-backward[buf]
  (re-backward buf #"((?<=\n)\n[^\n])"))

;(paragraph-forward {:str (rope "aaa\nbb") :pos 0 :y 0})
(defn char-forward[buf]
  (let [pos (buf :pos)
        newpos (inc pos)
        r (buf :str)
        ch (char-at r pos)]
    (if (= (or ch \newline) \newline)
      buf
      (buf-set-pos buf newpos))))

(defn char-backward[buf]
  (let [newpos (dec (buf :pos))
        r (buf :str)
        ch (char-at r newpos)]
    (if (= (or ch \newline) \newline)
      buf
      (buf-set-pos buf newpos))))

(defn pos-word[pos r]
  (let [re-start (re-pattern (str "([" not-word-chars "](?=[" word-chars "]))|((?<=[" not-word-chars "])$)"))
        re-end (re-pattern (str "[" word-chars "](?=[" not-word-chars "])"))
        ;_ (println r)
        ;_ (println re-start)
        ;_ (println re-end)
        b (or (last (pos-re+ pos r re-end)) (count r))
        a (or (last (pos-re- (dec b) r re-start)) 0)]
      [a b]))

;(pos-word 2 (rope "aaa"))

(defn current-word[buf]
  "return range of word under cursor, right side is exclusive"
  (let [{pos :pos
         r :str} buf]
    ;(println pos r)
    (pos-word pos r)))

(defn pos-re-forward-seq[pos r re]
  (if (neg? pos) nil
    (let [rg (pos-re+ pos r re)]
      (if (nil? rg) nil
        (cons rg (lazy-seq (pos-re-forward-seq (-> rg first inc) r re)))))))

(defn pos-re-backward-seq[pos r re]
  (if (neg? pos) nil
    (let [rg (pos-re- pos r re)]
      (if (nil? rg) nil
        (cons rg (lazy-seq (pos-re-backward-seq (-> rg first dec) r re)))))))

;(pos-re-forward-seq -1 (rope "(((") #"\(")
;(pos-re-backward-seq -1 (rope "(((") #"\(")
;(pos-re+ 0 (rope "   ()") #"\(|\)|\[|\]|\{|\}")

