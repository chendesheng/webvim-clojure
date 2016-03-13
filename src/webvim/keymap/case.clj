(ns webvim.keymap.case
  (:require [clojure.string :as str]
            [webvim.keymap.motion :refer [init-motion-keymap-fix-cw init-motion-keymap-for-operators]]
            [webvim.visual :refer [visual-block]]
            [webvim.core.line :refer [pos-line-start]]
            [webvim.keymap.operator :refer [wrap-operator inclusive? setup-range not-empty-range range-prefix]]
            [webvim.core.rope :refer [buf-set-pos buf-replace subr]]))

(defn- change-case [f]
  (fn [buf [a b]]
    buf [a b] 
    (-> buf
        (buf-replace a b
                     (-> buf
                         :str
                         (subr a b)
                         str
                         f))
        (buf-set-pos a))))


(defn swap-case [^String s]
  (str/join
    (map (fn [ch]
           (cond
             (Character/isUpperCase ch) (Character/toLowerCase ch)
             (Character/isLowerCase ch) (Character/toUpperCase ch)
             :else ch)) s)))

(defn wrap-keymap-case [keymap]
  (let [motion-keymap (init-motion-keymap-for-operators)]
    (-> keymap
        (update-in ["g"] assoc
                   "u" (merge
                         motion-keymap
                         {:after (wrap-operator (change-case clojure.string/lower-case))})
                   "U" (merge
                         motion-keymap
                         {:after (wrap-operator (change-case clojure.string/upper-case))}))
        (assoc "~" (merge
                     motion-keymap
                     {:after (wrap-operator (change-case swap-case))})))))

(defn- visual-change-case [f]
  (fn [buf keycode]
    (if (= (-> buf :visual :type) visual-block)
      (let [ranges (-> buf :visual :ranges)
            firstline (last ranges) ;ranges in reverse order
            r (buf :str)
            pos (buf :pos)
            newbuf (reduce
                     (fn [buf [a b]]
                       ((change-case f) buf [a (inc b)])) buf (not-empty-range ranges))]
        (-> newbuf
            (buf-set-pos (first firstline))
            ;leave visual mode
            (assoc-in [:context :cancel-visual-mode?] true)))
      (let [[a b :as rg] (range-prefix buf true)]
        (-> buf
            (buf-set-pos (pos-line-start (buf :str) a))
            ((change-case f) rg)
            (assoc-in [:context :cancel-visual-mode?] true))))))


(defn wrap-keymap-case-visual [keymap]
  (-> keymap
      (assoc
        "~" (visual-change-case swap-case)
        "u" (visual-change-case str/lower-case)
        "U" (visual-change-case str/upper-case))))

