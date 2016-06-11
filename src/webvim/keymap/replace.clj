(ns webvim.keymap.replace
  (:require [webvim.core.utils :refer [keycode-to-char nop]]
            [webvim.core.rope :refer [buf-set-pos subr buf-replace]]
            [webvim.core.event :refer [listen log]]
            [webvim.indent :refer [buf-indent-current-line]]
            [webvim.keymap.repeat :refer [repeat-pos-range]]
            [webvim.keymap.operator :refer [not-empty-range make-operator set-inclusive set-linewise set-visual-range]]))

(defn- replace-char [buf a b ch]
  (buf-replace buf a b
               (-> buf :str (subr a b) str
                   (.replaceAll "[^\r\n]" ch))))

(defn- replace-char-visual [buf keycode]
  (let [ch (keycode-to-char keycode)
        f (make-operator
            set-visual-range
            (fn [buf [a b]]
              (-> buf
                  (replace-char a b ch)
                  (buf-set-pos (buf :pos)))))]
    (f buf keycode)))

(defn- replace-char-visual-block [buf keycode]
  (let [ranges (-> buf :visual :ranges)
        firstline (last ranges) ;ranges in reverse order
        ch (keycode-to-char keycode)
        r (buf :str)
        pos (buf :pos)
        newbuf (reduce
                 (fn [buf [a b]]
                   (replace-char buf a (inc b) ch)) buf (not-empty-range ranges))]
    (buf-set-pos newbuf (first firstline))))

(defn- set-current-point-range [buf]
  (-> buf
      (update :context assoc :range (repeat-pos-range buf)) ;TODO: unicode char support
      (set-linewise false)
      (set-inclusive false)))

(defn- replace-char-normal [buf keycode]
  (let [ch (keycode-to-char keycode)
        f (make-operator
            set-current-point-range
            (fn [buf [a b]]
              (-> buf
                  (replace-char a b ch)
                  (buf-set-pos (dec b)))))]
    (f buf keycode)))

(defn wrap-keymap-replace [keymap]
  (assoc keymap "r" {"<esc>" nop
                     :else replace-char-normal}))

(listen
  :visual-mode-keymap
  (fn [keymap _]
    (assoc keymap "r" {"<esc>" nop
                       "<cr>" nop
                       :else (fn [buf keycode]
                               (if (-> buf :visual :type (= :visual-block))
                                 (replace-char-visual-block buf keycode)
                                 (replace-char-visual buf keycode)))})))
