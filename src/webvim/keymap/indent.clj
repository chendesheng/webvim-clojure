(ns webvim.keymap.indent
  (:require [webvim.keymap.motion :refer [init-motion-keymap-fix-cw init-motion-keymap-for-operators]]
            [webvim.keymap.compile :refer [wrap-keycode]]
            [webvim.keymap.operator :refer [make-linewise-operator inclusive? setup-range range-prefix
                                            set-linewise]]
            [webvim.indent :refer [buf-indent-current-line buf-indent-lines]]
            [webvim.core.rope :refer [buf-set-pos buf-replace subr re-test buf-insert rblank? char-at buf-delete]]
            [webvim.core.line :refer [line-start line-end pos-line pos-lines-seq+ pos-line-start]]
            [webvim.core.pos :refer [char- pos-re-seq+]]
            [webvim.core.utils :refer [repeat-chars nop]]))

(defn- indent-more [buf [a b]]
  (reduce
    (fn [buf [a b]]
      (buf-insert buf a "\t"))
    buf
    (filter 
      (fn [rg]
        (-> buf :str (subr rg) rblank? not))
      (reverse (pos-lines-seq+ (buf :str) a b)))))

(defn- count-leading-space [line]
  (let [[[a b]] (pos-re-seq+ line 0 #"^ *")]
    (- b a)))

(defn- indent-less [buf [a b]]
  (reduce
    (fn [{r :str pos :pos :as buf} [a b]]
      (let [line (subr r a b)]
        (if (= (char-at line 0) \tab)
          (buf-delete buf a (inc a))
          (buf-delete buf a (+ a (min (buf :tabsize) (count-leading-space line)))))))
    buf
    (reverse (pos-lines-seq+ (buf :str) a b))))

(defn wrap-keymap-indent [keymap]
  (let [motion-keymap (init-motion-keymap-for-operators)]
    (assoc keymap
           "=" (merge
                 motion-keymap
                 {"=" (wrap-keycode set-linewise)
                  :after (make-linewise-operator buf-indent-lines)})
           ">" (merge
                 motion-keymap
                 {:after (make-linewise-operator indent-more)})
           "<" (merge
                 motion-keymap
                 {:after (make-linewise-operator indent-less)}))))

(defn- indent [f]
  (fn [buf keycode]
    (let [[a b :as rg] (range-prefix buf true)]
      (-> buf
          (buf-set-pos (pos-line-start (buf :str) a))
          (f rg)))))

(defn wrap-keymap-indent-visual [keymap]
  (assoc keymap
         "=" (fn [buf keycode] (indent-range buf true))
         ">" (indent indent-more)
         "<" (indent indent-less)))
