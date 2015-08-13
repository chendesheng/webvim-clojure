;This namespace contains most global shared objects like active-buffer-id, buffer-list etc. as long as some common util functions
(ns webvim.global
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [clojure.java.io :as io])
  (:use clojure.pprint
        (clojure [string :only (join split)])))

;global registers. Don't access this directly, always access buffer's :registers
(defonce registers (atom {}))


;one server only serve one window at one time
;:scroll-top is a row number
(defonce window (atom{:viewport {:w 0 :h 0 :scroll-top 0}}))

;generate buffer id and buffer id only
(defonce gen-buf-id (atom 0))

;key: buffer id, value: buffer map
(defonce buffer-list (atom {}))

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

(defn cursor-inc-col [cursor]
  (update-in cursor [:col] inc))

(defn cursor-dec-col [cursor]
  (if (pos? (cursor :col))
    (update-in cursor [:col] dec)
    cursor))

(defn inc-col [b]
  (update-in b [:cursor] cursor-inc-col))

(defn dec-col [b]
  (if (pos? (-> b :cursor :col))
    (update-in b [:cursor :col] dec)
    b))

(defn col-count
  "Take row number return count length of a row."
  [b row]
  (count ((:lines b) row)))

(defn equal-pt?[pt1 pt2]
  (and (= (pt1 :col) (pt2 :col)) (= (pt1 :row) (pt2 :row))))

(defn quote-pattern[ch]
  (java.util.regex.Pattern/quote (str ch)))

(defn cursor-compare 
  "true if cur1 \"<=\" cur2"
  [{r1 :row c1 :col}
   {r2 :row c2 :col}]
  (or (< r1 r2) (and (= r1 r2) (<= c1 c2))))

(defn cursor-sort [cur1 cur2]
  (if (cursor-compare cur1 cur2)
    [cur1 cur2]
    [cur2 cur1]))

(defn cursor-sort-range [rg]
  (cursor-sort (rg 0) (rg 1)))

(defn re-test[re s]
  (if (nil? re)
    false
    (.find (re-matcher re s))))

(defn count-left-spaces[line]
  (count (re-find #"^\s*" line)))

(defn repeat-space[n]
  (reduce (fn[s _]
            (str s " ")) 
          ""
          (range 0 n)))
