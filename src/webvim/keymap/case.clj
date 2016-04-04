(ns webvim.keymap.case
  (:require [clojure.string :as str]
            [webvim.core.line :refer [pos-line-start]]
            [webvim.keymap.compile :refer [wrap-keycode]]
            [webvim.keymap.motion :refer [init-motion-keymap-fix-cw init-motion-keymap-for-operators]]
            [webvim.keymap.operator :refer [make-operator not-empty-range set-visual-range]]
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
        (update "g" assoc
                "u" (merge
                      motion-keymap
                      {:after (make-operator (change-case str/lower-case))})
                "U" (merge
                      motion-keymap
                      {:after (make-operator (change-case str/upper-case))}))
        (assoc "~" (merge
                     motion-keymap
                     {:after (make-operator (change-case swap-case))})))))

(defn- visual-change-case [f]
  (let [f (change-case f)
        fn-range (make-operator set-visual-range f)]
    (fn [buf keycode]
      (if (= (-> buf :visual :type) :visual-block)
        (let [ranges (-> buf :visual :ranges)
              firstline (last ranges) ;ranges in reverse order
              r (buf :str)
              pos (buf :pos)
              newbuf (reduce
                       (fn [buf [a b]]
                         (buf [a (inc b)])) buf (not-empty-range ranges))]
          (-> newbuf
              (buf-set-pos (first firstline))
              ;leave visual mode
              (assoc-in [:context :cancel-visual-mode?] true)))
        (-> buf
            (fn-range keycode)
            (assoc-in [:context :cancel-visual-mode?] true))))))


(defn wrap-keymap-case-visual [keymap]
  (-> keymap
      (assoc
        "~" (visual-change-case swap-case)
        "u" (visual-change-case str/lower-case)
        "U" (visual-change-case str/upper-case))))

