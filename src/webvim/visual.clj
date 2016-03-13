(ns webvim.visual
  (:require
    [webvim.core.rope :refer [subr]]
    [webvim.core.line :refer [make-linewise-range expand-block-ranges pos-line-last]]
    [webvim.core.utils :refer [sort2]]))

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

(defn set-visual-mode [buf visual]
  (-> buf
      (assoc :visual visual)
      set-visual-ranges))
