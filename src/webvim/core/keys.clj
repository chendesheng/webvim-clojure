(ns webvim.core.keys)

(defn- first-match[re s]
  (let [m (re-matcher re s)]
    (if (.find m)
      (let [a (.start m)
            b (.end m)]
        (subs s a b))
      nil)))

(defn input-keys 
  "parse input string to keys (lazy)."
  [s]
  (if (empty? s) '()
    (let [m (first-match #"^<.*?(?<!\\)>" s)]
      (if (nil? m)
        (let [m (first-match #"^\\." s)]
          (if (nil? m)
            (cons (str (first s)) 
                  (lazy-seq (input-keys (subs s 1))))
            (cons (str (second s)) 
                  (lazy-seq (input-keys (subs s 2))))))
        (cons (clojure.string/replace m #"\\(.)" "$1")
              (lazy-seq (input-keys (subs s (count m)))))))))

;(defn escape-keys [ks]
;  (clojure.string/replace ks #"([\\<>])", "\\$1"))
;
;(escape-keys "<>b")
