(ns webvim.autocompl
  (:require [clojure.set :as set])
  (:use clojure.pprint
        webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.buffer
        webvim.fuzzy
        (clojure [string :only (split)])))


;Keep reference count of each word: {"w1" 1 "w2" 3}
(defonce autocompl-words (atom {}))

(defn split-words 
  "split text to word vector"
   [txt]
  (split txt (re-pattern (str "[" not-word-chars "]")) -1))

;(split-words (rope "(ns [me.ray])"))

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

(defonce ^{:private true} listen-new-buffer
  (listen
    :new-buffer
    (fn [buf]
      (autocompl-words-parse (buf :str))
      (assoc buf 
             :autocompl 
             {:words nil
              ;empty suggestions means don't display it
              ;every input handled in insertion mode should check if :suggestion is nil.
              ;  if it is not nil then continue narrow down suggestions
              ;ctrl+n, ctrl+p will calculate full suggestions if it is nil
              :suggestions nil
              ;0 means selection nothing (don't highlight any suggestion item)
              ;> 0 means highlight the nth suggestion item
              :suggestions-index 0}))))

(defn expand-ends-word [s a b]
  (let [re-left (re-pattern (str "(?<=[" not-word-chars "])"))
        re-right (re-pattern (str "(?=[" not-word-chars "])"))]
    [(or (first (pos-re- s a re-left)) 0)
     (or (first (pos-re+ s b re-right)) (count s))]))

;(expand-ends-word (rope "aa   bb") 1 3)

(defn pos-uncomplete-word
  [s pos]
  (if (zero? pos) nil
    (let [re-start (re-pattern (str "(?<=[" not-word-chars "])"))
          a (or (last (pos-re- s pos re-start)) 0)]
      [a pos])))

(defn uncomplete-word
  [buf]
  (let [s (buf :str)
        pos (buf :pos)
        rg (pos-uncomplete-word s pos)]
    (if (or (nil? rg) (= (rg 0) (rg 1))) nil
        (str (subr s rg)))))

(defn buffer-replace-suggestion[buf word]
  (let [pos (buf :pos)
        s (buf :str)
        [a b] (pos-uncomplete-word s pos)]
    (buf-replace buf a b word)))

;(uncomplete-word {:pos 5 :str (rope " b cd")})

;(expand-ends-word (rope "aa bb") 1 3)
;(expand-ends-word (rope "aa ") 0 2)

(defn- autocompl-update
  [news olds c]
  (let [a (c :pos)
        oldb (-> c :len (+ a))
        newb (-> c :to count (+ a))]
    (autocompl-words-remove 
      (subr
        olds (expand-ends-word olds a oldb)))
    (autocompl-words-parse 
      (subr
        news (expand-ends-word news a newb)))))

(defonce ^{:private true} listen-change-buffer 
  (listen
    :change-buffer
    (fn [buf oldbuf c]
      (let [news (buf :str)
            olds (oldbuf :str)]
        (when-not (= news olds)
          (autocompl-update news olds c))
        buf))))
