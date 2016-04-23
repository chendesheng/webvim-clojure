(ns webvim.core.pos
  (:use webvim.core.rope
        webvim.core.event
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

(defn- find-last [m]
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
          (let [a (max 0 (- offset 50))
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

(defn pos-re-seq+ [r pos re]
  (let [rg (pos-re+ r pos re)]
    (if (some? rg)
      (cons rg (lazy-seq (pos-re-seq+ r (-> rg second) re))))))

(defn pos-re-seq-
  ([r m1 re-ahead pos re]
    (if (>= pos 0)
      (let [a (max 0 (- pos 50))
            b pos
            m (-> (.matcher r re-ahead)
                  (.useTransparentBounds true)
                  (.useAnchoringBounds false)
                  (.region a b))]
        (if (.find m)
          (let [_ (.find m1 (.start m))
                a (.start m1)
                b (.end m1)]
            (concat
              (loop [rgs '()]
                (if (.find m)
                  (do
                    (.find m1 (.start m))
                    (recur (conj rgs [(.start m1) (.end m1)])))
                  rgs))
              (cons [a b]
                    (lazy-seq
                      (pos-re-seq-
                        r m1 re-ahead
                        (dec a) re)))))
          (lazy-seq
            (pos-re-seq-
              r m1 re-ahead
              (dec a) re))))))
  ([r pos re]
    (let [re-ahead (re-pattern (str "(?=" re ")"))
          m1 (.useAnchoringBounds (.matcher r re) false)]
      (pos-re-seq- r m1 re-ahead pos re))))

(comment
  (pos-re-seq- (rope "12ab") 3 #".")
  ;about 10 times slower
  (defn slow-pos-re-seq- [r pos re]
    (let [rg (pos-re- r pos re)]
      (if (some? rg)
        (cons rg (lazy-seq (slow-pos-re-seq- r (-> rg first dec) re))))))

  (let [r (rope (clojure.string/join (repeat 10000 "defn")))]
    (time (def x (into '() (slow-pos-re-seq- r 40000 #"defn"))))
    (time (def x (into '() (pos-re-seq- r 40000 #"defn"))))))

(defn char+ [buf]
  (buf-move buf (fn [r pos]
                  (let [ch (or (char-at r pos) \newline)]
                    (cond
                      (= ch \newline)
                      pos
                      (or (surrogate? ch)
                          (variation-selector? (char-at r (inc pos))))
                      (+ pos 2)
                      :else
                      (inc pos))))))

(defn char- [buf]
  (buf-move buf (fn [r pos]
                  (let [newpos (dec pos)
                        ch (or (char-at r newpos) \newline)]
                    (if (= ch \newline)
                      pos newpos)))))

(defn buf-start [buf]
  (assoc buf :x 0 :y 0 :pos 0))

(defn buf-end [buf]
  (-> buf
      (assoc :y (-> buf :linescnt dec))
      (assoc :pos (-> buf :str count dec))))


(def brackets {\( \) \[ \] \{ \} \< \>})
(def all-brackets (into brackets (clojure.set/map-invert brackets)))
(def left-brackets (into #{} (keys brackets)))
(def right-brackets (into #{} (vals brackets)))
(def re-brackets (re-pattern (str "\\" (clojure.string/join "|\\" (keys all-brackets)))))

;copy from clojure.core/memoize
(defn- memoize-buf [f]
  (let [mem (atom {})]
    (listen
      :change-buffer
      (fn [buf oldbuf c]
        (reset! mem {})
        buf))
    (fn [& args]
      (if-let [e (find @mem args)]
        (val e)
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))

;FIXME: no regex recursion/balancing groups in java
(defonce ^:private not-begin-with-slash
  "(?<!(?<!(?<!(?<!(?<!\\\\)\\\\)\\\\)\\\\)\\\\)")

(defn- bracket? [r pos]
  (let [bracket (char-at r pos)
        m (all-brackets bracket)]
    (cond (nil? m) false
          (pos? pos)
          (let [matcher (.matcher r (re-pattern
                                      (str not-begin-with-slash (quote-pattern bracket))))]
            (.useTransparentBounds matcher true)
            (.region matcher pos (inc pos))
            (.find matcher))
          :else true)))

(def pos-match-bracket
  "return matched bracket position, nil if not find"
  (memoize-buf
    (fn [r pos]
      (if (bracket? r pos)
        (let [bracket (char-at r pos)
              m (all-brackets bracket)]
          (let [left? (contains? left-brackets bracket)
                inc-brackets (if left? left-brackets right-brackets)
                re (re-pattern (format "%s(%s)" not-begin-with-slash (quote-patterns bracket m)))]
            (loop [[[a _] & brackets] (if left?
                                        (pos-re-seq+ r pos re)
                                        (pos-re-seq- r pos re))
                   cnt 0]
              (if (nil? a) a
                  (let [newcnt (if (contains? inc-brackets (char-at r a))
                                 (inc cnt) (dec cnt))]
                    (if (zero? newcnt) a (recur brackets newcnt)))))))))))

