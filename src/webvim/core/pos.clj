(ns webvim.core.pos
  (:use webvim.core.rope
        webvim.core.utils))

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
  "return forward range matches. return nil if not found"
  [r pos re]
  (if (< pos (.length r))
    (let [m (.matcher r re)]
      (find-first m pos))))

(defn pos-re-
  "return backward range matches. Include current position. return nil if not found"
  [r pos re]
  (if-not (neg? pos) 
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
                (find-first m1 (first matches))))))))))

(defn buf-move 
  [buf fnmove]
  (let [{pos :pos
         r :str} buf
        newpos (or (fnmove r pos) pos)]
      (buf-set-pos buf newpos)))

(defn pos-re-seq+[r pos re]
  (let [rg (pos-re+ r pos re)]
    (if-not (nil? rg)
      (cons rg (lazy-seq (pos-re-seq+ r (-> rg second)re))))))

(defn pos-re-seq-[r pos re]
  (let [rg (pos-re- r pos re)]
    (if-not (nil? rg)
      (cons rg (lazy-seq (pos-re-seq- r (-> rg first dec) re))))))

;(pos-re-seq+ -1 (rope "(((") #"\(")
;(pos-re-seq- -1 (rope "(((") #"\(")
;(pos-re+ (rope "   ()") 0 #"\(|\)|\[|\]|\{|\}")

;(paragraph-forward {:str (rope "aaa\nbb") :pos 0 :y 0})

(defn char+[buf]
  (buf-move buf (fn [r pos]
                  (let [ch (or (char-at r pos) \newline)]
                    (if (= ch \newline)
                      pos (inc pos))))))

(defn char-[buf]
  (buf-move buf (fn [r pos]
                  (let [newpos (dec pos)
                        ch (or (char-at r newpos) \newline)]
                    (if (= ch \newline)
                      pos newpos)))))

(defn buf-start[buf]
  (merge buf {:x 0 :y 0 :pos 0}))

(defn buf-end[buf]
  (-> buf
      (assoc :y (-> buf :linescnt dec))
      (assoc :pos (-> buf :str count dec))))


(def braces {\( \) \[ \] \{ \} \< \>})
(def all-braces (into braces (clojure.set/map-invert braces)))
(def left-braces (into #{} (keys braces)))
(def right-braces (into #{} (vals braces)))
(def re-braces (re-pattern (str "\\" (clojure.string/join "|\\" (keys all-braces)))))

(defn pos-match-brace
  "return matched brace position, nil if not find"
  [r pos]
  (let [brace (char-at r pos)
        m (all-braces brace)
        left? (contains? left-braces brace)
        re (re-pattern (str  (quote-pattern brace) "|" (quote-pattern m)))]
    (if (nil? m) nil
      (let [inc-cnt? (if left?
                       #(contains? left-braces %)
                       #(contains? right-braces %))
            braces (if left?
                     (pos-re-seq+ r pos re)
                     (pos-re-seq- r pos re))
            mpos (reduce
                   (fn[cnt [a _]]
                     (let [ch (char-at r a)
                           newcnt (if (inc-cnt? ch)
                                    (inc cnt)
                                    (dec cnt))]
                       (if (zero? newcnt)
                         (reduced [a])
                         newcnt))) 0 braces)]
        (if (vector? mpos) (first mpos) nil)))))


