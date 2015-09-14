(ns webvim.autocompl
  (:require [clojure.set :as set])
  (:use clojure.pprint
        webvim.global
        webvim.buffer
        webvim.change
        webvim.text
        (clojure [string :only (split)])))

;Keep reference count of each word: {"w1" 1 "w2" 3}
(defonce autocompl-words (atom {}))

(defn split-words 
  "split text to word vector"
   [txt]
  (split txt #"[^a-zA-Z_]" -1))

(defn autocompl-parse
  "Split to word with length longer than 2."
  [txt]
  (reduce (fn[m w]
            (let [c (m w)]
              (if (nil? c)
                (assoc m w 1)
                (update-in m [w] inc)))) {} (filter #(> (count %) 2) (split-words txt))))

(defn- merge-words [m1 m2]
  (reduce-kv (fn [m w c]
               (let [c1 (or (m1 w) 0)]
                 (assoc m w (+ c1 c)))) m1 m2))

(defn- remove-words [m words]
  (reduce-kv
    (fn[m w diffcnt]
      (let [oldcnt (or (m w) 0)
            newcnt (- oldcnt diffcnt)]
        (if (pos? newcnt)
          (assoc m w newcnt)
          (dissoc m w)))) m words))

;(remove-words {"aa" 2 "bb" 1} {"aa" 3 "bb" 1 "cc" 2})

(defn autocompl-words-parse
  "add to autocompl-words"
  [txt]
  (swap! autocompl-words merge-words (autocompl-parse txt)))

(defn autocompl-words-remove[txt]
  (println "remove words:" (str txt))
  (swap! autocompl-words 
         remove-words 
         (autocompl-parse txt)))

(defn autocompl-suggest [subject]
  (reduce #(conj %1 (last %2)) []
          (sort-by (juxt first second str)
                   (reduce-kv 
                     (fn [suggestions word _] ;TODO sort by reference count?
                       (let [indexes (fuzzy-match word subject)]
                         (if (empty? indexes)
                           suggestions
                           (conj suggestions [(- (last indexes) 
                                                 (first indexes)) 
                                              (first indexes) word])))) 
                     [[0 0 subject]] (dissoc @autocompl-words subject)))))

(listen
  :new-buffer
  (fn [t]
    (autocompl-words-parse (t :str))
    t))

(defn expand-ends-word [s a1 b]
  (let [a (max (dec a1) 0)
        rg [a b]
        rga (pos-word a s)
        rgb (pos-word b s)]
    [(if (range-interact? rg rga)
       (min (first rga) a) a)
     (if (range-interact? rg rgb)
       (max (last rgb) b) b)]))

(defn uncomplete-word
  [t]
  (let [pos (t :pos)
        s (t :str)]
    (if (zero? pos) nil
      (let [re-start (re-pattern (str "[" not-word-chars "](?=[" word-chars "])"))
            a (or (last (pos-re-next-backward pos s re-start)) 0)]
        (println a)
        (str (text-subs s a pos))))))

;(uncomplete-word {:pos 5 :str (text-new " b cd")})

;(expand-ends-word (text-new "aa bb") 1 3)
;(expand-ends-word (text-new "aa ") 0 2)

(listen
  :change-buffer
  (fn [newt oldt]
    (let [c (-> newt :pending-undo :changes last)
          a (c :pos)
          olds (oldt :str)
          oldb (+ a (-> c :to count))
          news (newt :str)
          newb (+ a (c :len))]
      (autocompl-words-remove 
        (text-subs-range
          olds (expand-ends-word olds a oldb)))
      (autocompl-words-parse 
        (text-subs-range
          news (expand-ends-word news a newb)))
      newt)))
