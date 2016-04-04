(ns webvim.keymap.operator
  (:require 
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [webvim.mode :refer [set-insert-mode]]
    [webvim.keymap.compile :refer [wrap-key]]
    [webvim.core.rope :refer [buf-subr buf-set-pos buf-delete subr]]
    [webvim.core.line :refer [make-linewise-range expand-block-ranges
                              pos-line-last pos-line pos-line-end
                              pos-line-start line-start]]
    [webvim.core.register :refer [registers-delete-to! registers-yank-to! registers-put!]]
    [webvim.core.range :refer [range-exclusive range-linewise range-line-end]]
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
           "t" true "T" false "/" false "$" false "a" true
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

(defn- set-default-inclusive [buf keycode]
  (update buf :context
          update :inclusive?
          (fn [b]
            (if (nil? b) (inclusive? keycode) b))))

(defn- set-default-linewise [buf keycode]
  (update buf :context
          update :linewise?
          (fn [b]
            (if (nil? b) (linewise? keycode) b))))

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
      (let [buf (-> buf
                    fn-init
                    set-default-range
                    (set-default-inclusive keycode)
                    (set-default-linewise keycode))
            rg (get-operator-range buf)
            fn-set-pos (if (-> buf :context :linewise?)
                         line-start identity)]
        (-> buf
            ;This will make cursor position in right place after undo/redo. 
            (buf-set-pos (-> buf :context :range first)) 
            (fn-operator rg)
            fn-set-pos
            (update :context dissoc :linewise? :inclusive? :range)))))
  ([f]
    (make-operator identity f)))

(defn set-line-end [buf]
  (-> buf
      (set-range (range-line-end (buf :str) (buf :pos)))
      (set-inclusive false)))

(defn make-linewise-operator [f]
  (make-operator set-linewise f))

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

(defn yank-blockwise [buf items]
  (registers-put! (-> buf :context :register) {:str (str/join "\n" (map first items)) :blockwise? true}))

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


