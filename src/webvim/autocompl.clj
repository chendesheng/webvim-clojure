(ns webvim.autocompl
  (:require [clojure.set :as set])
  (:use clojure.pprint
        (clojure [string :only (split)])))

(defonce autocompl-words (atom #{}))

(defn fuzzy-match 
  "return true if word \"contains\" subject. length of subject must longer than 2"
  [word subject]
  (let [indexes (reduce (fn[indexes ch]
                          (let [i (.indexOf word (int ch) 
                                             (if (empty? indexes)
                                               0
                                               (-> indexes last inc)))]
                            (if (neg? i)
                              (reduced [])
                              (conj indexes i)))) [] subject)]
    (if (empty? indexes)
      []
      (let [rindexes (loop [i (-> indexes last dec)
                            j (-> subject count dec dec)
                            rindexes (list (last indexes))]
                       (cond 
                         (neg? i)
                         []
                         (neg? j)
                         rindexes
                         :else
                         (if (= (.charAt word i) (.charAt subject j)) 
                           (recur (dec i) (dec j) (conj rindexes i))
                           (recur (dec i) j rindexes))))]
        (if (= (count indexes) (count rindexes))
          rindexes
          indexes)))))


(defn split-words 
  "split text to word vector"
   [txt]
  (split txt #"[^a-zA-Z_]" -1))

(defn autocompl-parse
  "Split to word with length longer than 2"
  [txt]
  (set (filter #(> (count %) 2) (split-words txt))))

(defn autocompl-parse-buffer
  "Parse a buffer lines split to words and save to global autocompl-words"
  [b]
  (reset! autocompl-words 
          (reduce (fn[words line]
                    (set/union words (autocompl-parse line))) @autocompl-words (:lines b)))
  b)

(defn autocompl-suggest [words subject]
  (reduce #(conj %1 (last %2)) []
          (sort-by (juxt first second str)
                   (reduce 
                     (fn [suggestions word]
                       (let [indexes (fuzzy-match word subject)]
                         (if (empty? indexes)
                           suggestions
                           (conj suggestions [(- (last indexes) (first indexes)) (first indexes) word])))) [[0 0 subject]] words))))

