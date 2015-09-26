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
  [r pos re]
  (let [m (.matcher r re)]
    (find-first m pos)))

(defn pos-re-
  "return backward range matches"
  [r pos re]
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
        newpos (or (fnmove r pos) pos)]
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
              (or (first (pos-re+ r (inc pos) re)) 
                  (-> r count dec)))))

(defn re-backward[buf re]
  (buf-move buf 
            (fn[r pos]
              (or (first (pos-re- r (dec pos) re)) 0))))

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
(defn- move-char[buf incdec]
  (buf-move buf (fn [r pos]
                  (let [newpos (incdec pos)
                        ch (or (char-at r newpos) \newline)]
                    (if (= ch \newline)
                      pos newpos)))))

(defn char-forward[buf]
  (move-char buf inc))

(defn char-backward[buf]
  (move-char buf dec))

(defn pos-word[r pos]
  (let [re-start (re-pattern (str "([" not-word-chars "](?=[" word-chars "]))|((?<=[" not-word-chars "])$)"))
        re-end (re-pattern (str "[" word-chars "](?=[" not-word-chars "])"))
        b (or (last (pos-re+ r pos re-end)) (count r))
        a (or (last (pos-re- (dec b) r re-start)) 0)]
      [a b]))

;(pos-word (rope "aaa") 2)

(defn current-word[buf]
  "return range of word under cursor, right side is exclusive"
  (let [{pos :pos
         r :str} buf]
    ;(println pos r)
    (pos-word r pos)))

(defn pos-re-forward-seq[r pos re]
  (if (neg? pos) nil
    (let [rg (pos-re+ r pos re)]
      (if (nil? rg) nil
        (cons rg (lazy-seq (pos-re-forward-seq r (-> rg first inc)re)))))))

(defn pos-re-backward-seq[r pos re]
  (if (neg? pos) nil
    (let [rg (pos-re- r pos re)]
      (if (nil? rg) nil
        (cons rg (lazy-seq (pos-re-backward-seq r (-> rg first dec) re)))))))

;(pos-re-forward-seq -1 (rope "(((") #"\(")
;(pos-re-backward-seq -1 (rope "(((") #"\(")
;(pos-re+ (rope "   ()") 0 #"\(|\)|\[|\]|\{|\}")

