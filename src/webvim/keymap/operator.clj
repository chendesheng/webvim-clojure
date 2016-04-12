(ns webvim.keymap.operator
  (:require 
    [webvim.core.event :refer [log]]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [webvim.mode :refer [set-insert-mode]]
    [webvim.keymap.compile :refer [wrap-key]]
    [webvim.core.rope :refer [buf-subr buf-set-pos buf-delete subr]]
    [webvim.core.line :refer [make-linewise-range expand-block-ranges
                              pos-line-last pos-line pos-line-end
                              pos-line-start line-start]]
    [webvim.core.register :refer [registers-delete-to! registers-yank-to! registers-put!]]
    [webvim.core.range :refer [range-inclusive range-exclusive range-linewise range-line-end range-current-line]]
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

(defn linewise? [keycode]
  (contains? #{"j" "k" "+" "-" "G" "H" "M" "L"} keycode))

(defn inclusive? [keycode]
  (println "keycode:" keycode)
  (let [m {"h" false "l" false "w" false "W" false "e" true
           "E" true "b" false "B" false "f" true "F" false
           "t" true "T" false "/" false "$" false "a" true "^" false
           "i" true "{" false "}" false "0" false "n" false "N" false}]
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

(defn set-range [buf range]
  (update buf :context assoc :range range))

(defn set-linewise
  ([buf linewise?]
    (update buf :context assoc :linewise? linewise?))
  ([buf]
    (set-linewise buf true)))

(defn set-inclusive
  ([buf inclusive?]
    (update buf :context assoc :inclusive? inclusive?))
  ([buf]
    (set-inclusive buf true)))

(defn- set-default-range [buf]
  (update buf :context
          update :range
          (fn [rg]
            (or rg (sort2 (buf :pos)
                          (-> buf :context :lastbuf :pos))))))

(defmacro nilor
  "Like `or` but check nil?"
  ([] nil)
  ([x] x)
  ([x & next]
    `(let [or# ~x]
       (if-not (nil? or#) or# (or ~@next)))))

(defn set-default-inclusive [buf keycode]
  (update-in buf [:context :inclusive?] #(nilor % (inclusive? keycode))))

(defn- set-default-linewise [buf keycode]
  (update-in buf [:context :linewise?] #(nilor % (linewise? keycode))))

;return range for operator, always characterwise and exclusive
(defn- get-operator-range [{r :str
                            {linewise? :linewise?
                             inclusive? :inclusive?
                             rg :range} :context :as buf}]
  (pprint {:linewise? linewise?
           :inclusive? inclusive?
           :rang rg})
  (if linewise?
    (range-linewise r rg)
    (if inclusive? (range-exclusive rg) rg)))

(defn make-operator
  ([fn-init fn-operator]
    (fn [buf keycode]
      (log (-> buf :context :inclusive?))
      (let [buf (-> buf
                    fn-init
                    set-default-range
                    (set-default-inclusive keycode)
                    (set-default-linewise keycode))
            rg (get-operator-range buf)
            fn-set-pos (if (-> buf :context :linewise?)
                         line-start identity)]
        (log "make-operator")
        (log [rg (-> buf :context :linewise?)])
        (-> buf
            ;This will make cursor position in right place after undo/redo. 
            (buf-set-pos (first rg)) 
            (fn-operator rg)
            fn-set-pos
            (update :context dissoc :linewise? :inclusive? :range)))))
  ([f]
    (make-operator identity f)))

(defn set-visual-range [{r :str {rg :range typ :type} :visual :as buf}]
  (log "set-visual-range")
  (-> buf
      (set-linewise (= typ :visual-line))
      (set-inclusive true)
      (set-range (sort2 rg))))

(defn set-line-end [buf]
  (-> buf
      (set-range (range-line-end (buf :str) (buf :pos)))
      (set-inclusive false)))

(defn set-current-line [buf]
  (-> buf
      (set-range (range-current-line (buf :str) (buf :pos)))
      (set-linewise false)
      (set-inclusive false)))

(defn make-linewise-operator
  ([fn-init f]
    (make-operator (comp set-linewise fn-init) f))
  ([f]
    (make-linewise-operator identity f)))

(defn make-operator-current-line [f]
  (fn [{r :str pos :pos :as buf} keycode]
    (-> buf
        set-linewise
        (f (range-linewise r pos pos))
        (update :context dissoc :linewise?))))

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

(defn set-visual-ranges [{r :str
                          {tp :type rg :range} :visual
                          :as buf}]
  (println "set-visual-ranges:" (range-linewise r rg))
  ;(.printStackTrace (Exception.))
  (assoc-in buf [:visual :ranges]
            (condp = tp
              :visual-range (list (sort2 rg))
              :visual-line (list (range-linewise r rg))
              :visual-block (into '() (expand-block-ranges r rg (buf :tabsize)))
              nil)))

(defn visual-block-lines [buf]
  (let [buf (set-visual-ranges buf)]
    (reduce (fn [items [a b]]
              (let [eol (dec (pos-line-last (buf :str) a))
                    b (min eol (inc b))]
                (conj items [(-> buf :str (subr a b)) a b]))) [] (-> buf :visual :ranges))))

(defn ignore-by-keycode [f pred]
  (fn [buf keycode]
    (if (pred keycode)
      buf
      (f buf keycode))))

