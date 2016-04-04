(ns webvim.keymap.yank
  (:require
    [webvim.core.utils :refer [nop]]
    [webvim.core.rope :refer [buf-set-pos buf-subr]]
    [webvim.core.register :refer [registers-yank-to! registers-delete-to!]]
    [webvim.core.range :refer [range-linewise]]
    [webvim.keymap.compile :refer [wrap-keycode]]
    [webvim.keymap.motion :refer [init-motion-keymap-for-operators]]
    [webvim.keymap.operator :refer [visual-block-lines yank-blockwise make-operator
                                    make-operator-current-line
                                    make-linewise-operator set-linewise set-range]]))

(defn yank-range
  ([buf [a b]]
    (let [s (buf-subr buf a b)]
      (registers-yank-to! (-> buf :context :register) {:str s :linewise? (-> buf :context :linewise?)})
      (update buf :context dissoc :register))))

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

(defn wrap-keymap-yank [keymap]
  (let [motion-keymap (init-motion-keymap-for-operators)
        fn-yank (make-operator yank-range)]
    (assoc keymap
           "y" (merge
                 motion-keymap
                 {"y" (make-operator-current-line yank-range)
                  :after (fn [buf keycode]
                           (if (not= keycode "y")
                             (fn-yank buf keycode)
                             buf))})
           "Y" (make-operator-current-line yank-range))))

(defn wrap-keymap-yank-visual [keymap]
  (assoc keymap "y" visual-keymap-y))
