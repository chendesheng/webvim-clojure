(ns webvim.keymap.operator
  (:require 
    [clojure.string :as str]
    [webvim.core.rope :refer [buf-subr]]
    [webvim.core.line :refer [pos-line pos-line-last]]
    [webvim.core.register :refer [registers-delete-to! registers-yank-to! registers-put!]]
    [webvim.keymap.action :refer [range-prefix]]))

(defn setup-range [buf]
  (println "setup-range:" (-> buf :context :range))
  (if (-> buf :context :range nil?)
    (let [pos (buf :pos)
          lastbuf (-> buf :context :lastbuf)
          lastpos (lastbuf :pos)]
      (-> buf
          ;Restore positoin to lastbuf so that changes happen next can record correct start position. This will make cursor position in right place after undo/redo.
          (merge (select-keys lastbuf [:pos :x :y]))
          (assoc-in [:context :range] [lastpos pos]))) buf))

(defn inclusive? [keycode]
  (println "keycode:" keycode)
  (let [m {"h" false "l" false "w" false "W" false "e" true
           "E" true "b" false "B" false "f" true "F" false
           "t" true "T" false "/" false "$" false "a" true
           "i" true}]
    (if (contains? m keycode)
      (m keycode)
      true)))

(defn wrap-operator [f]
  (fn [buf keycode]
    (let [buf (setup-range buf)
          rg (range-prefix buf (inclusive? keycode))]
      (f buf rg))))

(defn not-empty-range [ranges]
  (filter (fn [[a b]]
            (< a (inc b))) ranges))

(defn- buf-yank
  ([buf a b linewise? delete?]
    (let [s (buf-subr buf a b)]
      ((if delete?
         registers-delete-to!
         registers-yank-to!) (-> buf :context :register) {:str s :linewise? linewise?})
      (update-in buf [:context] dissoc :register)))
  ([buf a b linewise?]
    (buf-yank buf a b linewise? false)))

(defn yank-blockwise [buf items]
  (registers-put! (buf :register) {:str (str/join "\n" (map first items)) :blockwise? true}))

(defn yank-range [buf inclusive? linewise?]
  (let [[a b] (range-prefix buf inclusive?)]
    (buf-yank buf a b linewise?)))

(defn setup-range-line [buf]
  (assoc-in buf [:context :range] (pos-line (buf :str) (buf :pos))))

