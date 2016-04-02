(ns webvim.autocompl
  (:require [clojure.set :as set])
  (:use clojure.pprint
        webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.buffer
        webvim.core.lang
        webvim.fuzzy
        (clojure [string :only (split)])))

(println "load autocompl language")

;Keep reference count of each word: {"w1" 1 "w2" 3}
(defonce autocompl-words (agent {}
                                :error-handler
                                (fn [_ err]
                                  (println "autocompl-words agent failed:")
                                  (println err))))

(defn- not-word-chars [lang]
  (-> lang word-re :not-word-chars))

(defn- split-words 
  "split text to word vector"
  [lang txt]
  (split txt (re-pattern (str "[" (not-word-chars lang) "]")) -1))

;(split-words (rope "(ns [me.ray])"))

(defn- autocompl-parse
  "Split to word with length longer than 2."
  [lang txt]
  (frequencies (filter #(> (count %) 2) (split-words lang txt))))

(defn- merge-words [m1 m2]
  (reduce-kv (fn [m w c]
               (let [c1 (or (m1 w) 0)]
                 (assoc m w (+ c1 c)))) m1 m2))

(defn- remove-words [m words]
  (reduce-kv
    (fn [m w diffcnt]
      (let [oldcnt (or (m w) 0)
            newcnt (- oldcnt diffcnt)]
        (if (pos? newcnt)
          (assoc m w newcnt)
          (dissoc m w)))) m words))

;(remove-words {"aa" 2 "bb" 1} {"aa" 3 "bb" 1 "cc" 2})

(defn autocompl-remove-word [words w]
  (remove-words words {w 1}))

(defn autocompl-add-word [words w]
  (update words w (fn [n]
                    (inc (or n 0)))))

(listen
  :new-buffer
  (fn [buf]
    (println "autocompl new-buffer")
    (send autocompl-words
          (fn [words]
            (merge-words words
                         (autocompl-parse (buf :language) (buf :str)))))
    buf))

(defn expand-ends-word [lang s a b]
  (let [re-left (re-pattern (str "(?<=[" (not-word-chars lang) "])"))
        re-right (re-pattern (str "(?=[" (not-word-chars lang) "])"))]
    [(or (first (pos-re- s a re-left)) 0)
     (or (first (pos-re+ s b re-right)) (count s))]))

;(expand-ends-word (rope "aa   bb") 1 3)

(defn pos-uncomplete-word
  [lang s pos]
  (if (zero? pos) nil
      (let [re-start (re-pattern (str "(?<=[" (not-word-chars lang) "])"))
            a (or (last (pos-re- s pos re-start)) 0)]
        [a pos])))

(defn buffer-uncomplete-word
  [buf]
  (let [s (buf :str)
        pos (buf :pos)
        lang (buf :language)
        rg (pos-uncomplete-word lang s pos)]
    (if (or (nil? rg) (= (rg 0) (rg 1))) nil
        (str (subr s rg)))))

(defn buffer-replace-suggestion [buf word oldword]
  (if (= word oldword) buf
      (let [pos (buf :pos)
            s (buf :str)
            lang (buf :language)]
        (buf-replace buf (- pos (count oldword)) pos word))))

;(uncomplete-word {:pos 5 :str (rope " b cd")})

;(expand-ends-word (rope "aa bb") 1 3)
;(expand-ends-word (rope "aa ") 0 2)

(defn- autocompl-update
  [lang news olds c]
  (send autocompl-words
        (fn [words]
          (let [a (c :pos)
                oldb (-> c :len (+ a))
                newb (-> c :to count (+ a))]
            (-> words
                (remove-words 
                  (autocompl-parse 
                    lang (subr olds (expand-ends-word lang olds a oldb))))
                (merge-words
                  (autocompl-parse
                    lang (subr
                           news (expand-ends-word lang news a newb)))))))))

(listen
  :change-buffer
  (fn [{lang :language
        news :str
        :as buf}
       {olds :str
        :as oldbuf} c]
    (when-not (= news olds)
      (autocompl-update
        lang news olds c))
    buf))

(listen
  :close-buffer
  (fn [buf]
    (send autocompl-words
          (fn [words]
            (remove-words words (autocompl-parse (buf :language) (buf :str)))))
    buf))
