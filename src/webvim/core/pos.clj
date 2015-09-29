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
  "return forward range matches. return nil if not found"
  [r pos re]
  (if (< pos (.length r))
    (let [m (.matcher r re)]
      (find-first m pos))))

(defn pos-re-
  "return backward range matches. return nil if not found"
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

(defn buf-start[buf]
  (merge buf {:x 0 :y 0 :pos 0}))

(defn buf-end[buf]
  (-> buf
      (assoc :y (-> buf :linescnt dec))
      (assoc :pos (-> buf :str count dec))))
