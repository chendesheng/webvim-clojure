;This namespace contains most global shared objects like active-buffer-id, buffer-list etc. as long as some common util functions
(ns webvim.global
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            [snipsnap.core :as clipboard])
  (:use clojure.pprint
        (clojure [string :only (join split)])))

;global registers. Don't access this directly, always access buffer's :registers
(defonce registers (atom {}))

(defn registers-get [regs ch]
  (if (= ch "+")
    (clipboard/get-text)
    (@regs ch)))

(defn registers-put [regs ch text]
  (if (= ch "+")
    (clipboard/set-text! text)
    (swap! regs assoc ch text)))

;one server only serve one window at one time
(defonce window (atom{:viewport {:w 0 :h 0}}))

;generate buffer id and buffer id only
(defonce gen-buf-id (atom 0))

;key: buffer id, value: buffer map
(defonce buffer-list (atom {}))

(defonce normal-mode 0)
(defonce insert-mode 1)
(defonce visual-mode 2)
(defonce ex-mode 3)

;the current editting buffer, when receving keys from client send keycode to this buffer's :chan-in
;switch to another buffer is very easy, just do (reset! actvive-buffer b)
;TODO Serve more than one client at once, make eash session different active-buffer
(defonce active-buffer-id (atom int))

(defn active-buffer[]
  (@buffer-list @active-buffer-id))

(defn pprint2
  "call pprint and return first argument, works with in \"->\" macro"
  ;TODO find out a way to print out caller's function name and file name so that we can get rid of the prefix argument.
  ;TODO get a better name
  [obj prefix]
  (let[]
    (println prefix)
    (pprint obj)
    obj))

(defn bound-range[v s e]
  (cond (<= v s) s
        (>= v e) e
        :else v))

(defn quote-pattern[ch]
  (java.util.regex.Pattern/quote (str ch)))

(defn sort2[a b]
  (if (< a b) [a b] [b a]))

(defn delta[a b]
  (if (< a b)
    (- b a)
    (- a b)))

(defn repeat-space[n]
  (reduce (fn[s _]
            (str s " ")) 
          ""
          (range 0 n)))
 
(defn fuzzy-match 
  "return char indexs if word \"contains\" subject. length of subject must longer than 2"
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

(defn call-if-fn [b f & args]
  (if (fn? f)
    (apply f b args)
    b))

(def left-braces #{\( \[ \{})
(def right-braces #{\) \] \}})
(def all-braces {\( \) \) \( \[ \] \] \[ \{ \} \} \{})

(def re-braces #"(?<!\\)(\(|\[|\{|\}|\]|\))")

;http://stackoverflow.com/questions/21191045/get-string-indices-from-the-result-of-re-seq
(defn re-seq-pos [pattern string start] 
  (let [m (re-matcher pattern (subs string start))] 
    ((fn step [] 
      (when (. m find) 
        (cons {:start (+ (. m start) start) :end (+ (. m end) start) :group (. m group)} 
          (lazy-seq (step))))))))

