(ns webvim.text
  (:use clojure.pprint
        (clojure [string :only (join split blank?)])))

(def line-break "\n")

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
      (if (zero? offset)
        nil
        (let[[a b] (if (< offset 1)
                     [0 offset]
                     [(- offset 1) offset])]
          (.region m a b)
          (let [matches (find-last m)]
            (if (or (nil? matches)
                    (>= (first matches) b))
              (recur a)
              (find-first m1 (first matches)))))))))

(def re-WORD-start #"(?<=\s)\S")
(re-backward "\nhello\n  abc" 8 #"\n")

(defn build-text
  [^String s]
  {:s s  ;string or rope
   :pos {:offset 0 ;offset from first char
         :x 0      ;offset from previous line break
         :y 0}})   ;num of line breaks from first char

(defn count-lines[s]
  (let [cnt (count line-break)]
    (loop[s1 s n 0]
      (let [i (.indexOf s1 line-break)]
        (if (= i -1)
          n
          (recur (subs s1 (+ i cnt)) (inc n)))))))

(defn text-update-pos
  [t offset]
  (let [pos (t :pos)
        a (pos :offset)
        b (+ a offset)
        sub (text-subs a b)
        lastEOL (re-backward (t :s) b)]
    (assoc t :pos 
           {:offset b
            :x (if (nil? lastEOL)
                 b
                 (- b (last lastEOL)))
            :y (+ (pos :y) (count-lines sub))})))
