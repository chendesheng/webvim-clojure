(ns webvim.keymap.replace
  (:require [webvim.core.utils :refer [keycode-to-char nop]]
            [webvim.core.rope :refer [buf-set-pos subr buf-replace]]
            [webvim.core.event :refer [listen]]
            [webvim.indent :refer [buf-indent-current-line]]
            [webvim.keymap.operator :refer [not-empty-range make-operator set-inclusive set-visual-range]]))

(defn- replace-char [buf a b ch]
  (buf-replace buf a b
               (-> buf :str (subr a b) str
                   (.replaceAll "[^\r\n]" ch))))

(defmulti replace-char-keycode
  (fn [buf keycode]
    (cond
      (not= (count (keycode-to-char keycode)) 1)
      :nop
      (= :visual-block (-> buf :visual :type))
      :visual-block
      :else
      :not-visual-block)))

(defmethod replace-char-keycode :nop [buf keycode]
  buf)

(defmethod replace-char-keycode :not-visual-block [buf keycode]
  (let [ch (keycode-to-char keycode)
        f (make-operator set-visual-range
                         (fn [buf [a b]]
                           (replace-char buf a b ch)))]
    (-> buf
        (f keycode)
        (buf-set-pos (buf :pos)))))

(defmethod replace-char-keycode :visual-block [buf keycode]
  (let [ranges (-> buf :visual :ranges)
        firstline (last ranges) ;ranges in reverse order
        ch (keycode-to-char keycode)
        r (buf :str)
        pos (buf :pos)
        newbuf (reduce
                 (fn [buf [a b]]
                   (replace-char buf a (inc b) ch)) buf (not-empty-range ranges))]
    (buf-set-pos newbuf (first firstline))))

(defn wrap-keymap-replace [keymap]
  (assoc keymap "r" {"<esc>" nop
                     :else replace-char-keycode}))

(listen
  :visual-mode-keymap
  (fn [keymap _]
    (assoc keymap "r" {"<esc>" nop
                       "<cr>" nop
                       :else replace-char-keycode})))
