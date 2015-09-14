(ns webvim.autocompl
  (:require [clojure.set :as set])
  (:use clojure.pprint
        webvim.global
        webvim.buffer
        (clojure [string :only (split)])))

;Keep reference count of each word: {"w1" 1 "w2" 3}
(defonce autocompl-words (atom {}))
;init

(listen :buf-inited 
        (fn init-words[b]
          (reset! autocompl-words 
                  (autocompl-parse (b :str)))))

(listen :str-change (fn[t] t))

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

(defn autocompl-words-parse-lines
  "Parse a buffer lines split to words and save to global autocompl-words"
  [lines]
  (reset! autocompl-words 
          (reduce (fn[words line]
                    (merge-words words (autocompl-parse line))) @autocompl-words lines)))

(defn autocompl-words-remove[txt]
  (swap! autocompl-words 
         remove-words 
         (autocompl-parse txt)))

(defn autocompl-words-parse
  "add to autocompl-words"
  [txt]
  (swap! autocompl-words merge-words (autocompl-parse txt)))

(defn autocompl-words-remove-lines[lines]
  (reset! autocompl-words 
          (reduce (fn[words line]
                    (remove-words words (autocompl-parse line))) @autocompl-words lines)))

(defn autocompl-suggest [words subject]
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
                     [[0 0 subject]] (dissoc words subject)))))
