(ns webvim.keymap.indent
  (:require [webvim.keymap.motion :refer [init-motion-keymap-fix-cw init-motion-keymap-for-operators]]
            [webvim.keymap.compile :refer [wrap-keycode]]
            [webvim.keymap.operator :refer [make-linewise-operator set-linewise set-visual-range if-not-keycode]]
            [webvim.keymap.repeat :refer [wrap-keymap-repeat-prefix]]
            [webvim.keymap.visual :refer [wrap-temp-visual-mode keycodes-visual]]
            [webvim.indent :refer [buf-indent-current-line buf-indent-lines]]
            [webvim.core.rope :refer [buf-set-pos buf-replace subr re-test buf-insert rblank? char-at buf-delete]]
            [webvim.core.event :refer [listen]]
            [webvim.core.line :refer [line-start line-end pos-line pos-lines-seq+ pos-line-start]]
            [webvim.core.pos :refer [char- pos-re-seq+]]
            [webvim.core.utils :refer [repeat-chars nop deep-merge]]))

(defn- indent-more [buf [a b]]
  (reduce
    (fn [buf [a b]]
      (buf-insert buf a "\t"))
    buf
    (filter 
      (fn [rg]
        (-> buf :str (subr rg) rblank? not))
      (reverse (pos-lines-seq+ buf a b)))))

(defn- count-leading-space [line]
  (let [[[a b]] (pos-re-seq+ line 0 #"^ *")]
    (- b a)))

(defn- indent-less [buf [a b]]
  (println a b)
  (reduce
    (fn [{r :str pos :pos :as buf} [a b]]
      (let [line (subr r a b)]
        (if (= (char-at line 0) \tab)
          (buf-delete buf a (inc a))
          (buf-delete buf a (+ a (min (buf :tabsize) (count-leading-space line)))))))
    buf
    (reverse (pos-lines-seq+ buf a b))))

(defn temp-visual-mode [visual-keymap f]
  (wrap-temp-visual-mode visual-keymap
                         #((make-linewise-operator f) %1 %2)))


(defn wrap-keymap-indent [keymap visual-keymap]
  (let [motion-keymap (init-motion-keymap-for-operators)]
    (assoc keymap
           "=" (wrap-keymap-repeat-prefix
                 (deep-merge
                   motion-keymap
                   (temp-visual-mode visual-keymap buf-indent-lines)
                   {"=" nop
                    :after (-> buf-indent-lines
                               make-linewise-operator
                               (if-not-keycode keycodes-visual))}))
           ">" (wrap-keymap-repeat-prefix
                 (deep-merge
                   motion-keymap
                   (temp-visual-mode visual-keymap indent-more)
                   {:after (-> indent-more
                               make-linewise-operator
                               (if-not-keycode keycodes-visual))}))
           "<" (wrap-keymap-repeat-prefix
                 (deep-merge
                   motion-keymap
                   (temp-visual-mode visual-keymap indent-less)
                   {:after (-> indent-less
                               make-linewise-operator
                               (if-not-keycode keycodes-visual))})))))

(listen
  :visual-mode-keymap
  (fn [keymap _]
    (assoc keymap
           "=" (make-linewise-operator set-visual-range buf-indent-lines)
           ">" (make-linewise-operator set-visual-range indent-more)
           "<" (make-linewise-operator set-visual-range indent-less))))
