(ns webvim.keymap.operator
  (:require 
    [clojure.string :as str]
    [webvim.mode :refer [set-insert-mode]]
    [webvim.keymap.compile :refer [wrap-key]]
    [webvim.core.rope :refer [buf-subr buf-set-pos buf-delete subr]]
    [webvim.core.line :refer [make-linewise-range expand-block-ranges pos-line-last pos-line pos-line-end]]
    [webvim.core.register :refer [registers-delete-to! registers-yank-to! registers-put!]]
    [webvim.core.utils :refer [make-range sort2 nop]]))

(defn setup-range [buf]
  ;(println "setup-range:" (-> buf :context :range))
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

;collect range argument, TODO: add linewise
(defn range-prefix [{{tp :type rg :range} :visual :as buf} inclusive?]
  (cond
    (= tp :visual-range)
    (make-range rg inclusive?)
    (= tp :visual-line)
    (make-linewise-range rg buf)
    (= tp :visual-block)
    (throw (Exception. "TODO: visual-block"))
    (-> buf :context :range nil? not)
    (-> buf :context :range (make-range inclusive?))
    :else (throw (Exception. "no range prefix exist"))))

(defn wrap-operator [f]
  (fn [buf keycode]
    (let [buf (setup-range buf)
          rg (range-prefix buf (inclusive? keycode))]
      (f buf rg))))

(defn not-empty-range [ranges]
  (filter (fn [[a b]]
            (< a (inc b))) ranges))

(defn buf-yank
  ([buf a b linewise? delete?]
    (let [s (buf-subr buf a b)]
      ((if delete?
         registers-delete-to!
         registers-yank-to!) (-> buf :context :register) {:str s :linewise? linewise?})
      (update buf :context dissoc :register)))
  ([buf a b linewise?]
    (buf-yank buf a b linewise? false)))

(defn yank-blockwise [buf items]
  (registers-put! (buf :register) {:str (str/join "\n" (map first items)) :blockwise? true}))

;delete [a b) shift pos
(defn- shift-delete [pos a b]
  (cond
    (and (<= a pos) (< pos b)) a
    (>= pos b) (- pos (- b a))
    :else pos))

(defn- shift-ranges-delete [ranges a b]
  (if (< a b)
    (map 
      (fn [[a1 b1]]
        [(shift-delete a1 a b)
         (shift-delete b1 a b)])
      ranges) ranges))

(defn set-visual-ranges [{{tp :type rg :range} :visual :as buf}]
  ;(println "set-visual-ranges:" tp rg)
  (assoc-in buf [:visual :ranges]
            (condp = tp
              :visual-range (list (sort2 rg))
              :visual-line (list (make-linewise-range rg buf))
              :visual-block (into '() (expand-block-ranges (buf :str) rg (buf :tabsize)))
              nil)))

(defn visual-block-lines [buf]
  (let [buf (-> buf
                set-visual-ranges)]
    (reduce (fn [items [a b]]
              (let [eol (dec (pos-line-last (buf :str) a))
                    b (min eol (inc b))]
                (conj items [(-> buf :str (subr a b)) a b]))) [] (-> buf :visual :ranges))))

(defn visual-block-delete [buf]
  (let [items (visual-block-lines buf)
        buf (buf-set-pos buf (-> items last (get 1)))]
    (yank-blockwise buf (rseq items))
    (reduce (fn [buf [_ a b]]
              (-> buf
                  (update-in [:visual :ranges] shift-ranges-delete a b)
                  (buf-delete a b))) buf items)))


(defn yank-range [buf inclusive? linewise?]
  (let [[a b] (range-prefix buf inclusive?)]
    (buf-yank buf a b linewise?)))

(defn setup-range-line [buf]
  (assoc-in buf [:context :range] (pos-line (buf :str) (buf :pos))))

(defn setup-range-line-end [buf]
  (let [a (buf :pos)
        b (pos-line-end (buf :str) a)]
    (assoc-in buf [:context :range] [a b])))

(defn delete-char [buf]
  (let [pos (buf :pos)
        [a b] [pos (inc pos)]]
    (buf-yank buf a b false true)
    (buf-delete buf a b)))

;for cv{motion}, dv{motion} etc.
(defn wrap-temp-visual-mode [visual-keymap f]
  (let [visual-keymap (wrap-key visual-keymap
                                :leave (fn [handler]
                                         (fn [buf keycode]
                                           (let [f (if (= keycode "<esc>") nop f)]
                                             (-> buf
                                                 (f keycode)
                                                 (handler keycode))))))]
    {"v" visual-keymap
     "V" visual-keymap
     "<c-v>" visual-keymap}))
