(ns webvim.keymap.yank
  (:require
    [webvim.core.utils :refer [nop]]
    [webvim.core.rope :refer [buf-set-pos]]
    [webvim.keymap.motion :refer [init-motion-keymap-for-operators]]
    [webvim.keymap.operator :refer [visual-block-lines yank-range yank-blockwise setup-range-line setup-range inclusive?]]))

(defn- visual-block-yank [buf]
  (let [items (visual-block-lines buf)
        buf (buf-set-pos buf (-> items last (get 1)))]
    (yank-blockwise buf (rseq items))
    buf))

(defmulti visual-keymap-y (fn [buf keycode] (-> buf :visual :type)))
(defmethod visual-keymap-y :visual-range [buf keycode]
  (yank-range buf true false))
(defmethod visual-keymap-y :visual-line [buf keycode]
  (yank-range buf true true))
(defmethod visual-keymap-y :visual-block [buf keycode]
  (visual-block-yank buf))

(defn- yank [buf keycode]
  (if (contains? #{"y" "j" "k" "Y"} keycode)
    (-> buf
        setup-range-line
        (yank-range false true))
    (-> buf
        setup-range
        (yank-range (inclusive? keycode) false))))

(defn wrap-keymap-yank [keymap]
  (let [motion-keymap (init-motion-keymap-for-operators)]
    (assoc keymap
           "y" (merge
                 motion-keymap
                 {"y" nop
                  :after yank})
           "Y" yank)))

(defn wrap-keymap-yank-visual [keymap]
  (assoc keymap "y" visual-keymap-y))
