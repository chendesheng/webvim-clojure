(ns webvim.keymap.yank
  (:require
    [clojure.string :as string]
    [webvim.core.utils :refer [nop]]
    [webvim.core.rope :refer [buf-set-pos buf-subr]]
    [webvim.core.register :refer [registers-yank-to! registers-put!]]
    [webvim.core.range :refer [range-linewise]]
    [webvim.core.event :refer [listen]]
    [webvim.keymap.compile :refer [wrap-keycode]]
    [webvim.keymap.visual :refer [wrap-temp-visual-mode keycodes-visual]]
    [webvim.keymap.motion :refer [init-motion-keymap-for-operators]]
    [webvim.keymap.operator :refer [visual-block-lines make-operator ignore-by-keycode
                                    make-operator-current-line make-linewise-operator
                                    set-linewise set-range set-visual-range]]))

(defn yank-blockwise [buf items]
  (registers-put! (-> buf :context :register) {:str (string/join "\n" (map first items)) :blockwise? true}))

(defn yank-range
  ([buf [a b]]
    (let [s (buf-subr buf a b)]
      (registers-yank-to! (-> buf :context :register)
                          {:str s :linewise? (-> buf :context :linewise?)})
      (update buf :context dissoc :register))))

(defn- visual-block-yank [buf]
  (let [items (visual-block-lines buf)
        buf (buf-set-pos buf (-> items last (get 1)))]
    (yank-blockwise buf (rseq items))
    buf))

(defn- visual-keymap-y [buf keycode]
  (if (= (-> buf :visual :type) :visual-block)
    (visual-block-yank buf)
    ((make-operator set-visual-range yank-range) buf keycode)))

(defn wrap-keymap-yank [keymap visual-keymap]
  (let [motion-keymap (init-motion-keymap-for-operators)
        fn-yank (make-operator yank-range)]
    (assoc keymap
           "y" (merge
                 motion-keymap
                 (wrap-temp-visual-mode visual-keymap visual-keymap-y)
                 {"y" (make-operator-current-line yank-range)
                  :after (ignore-by-keycode fn-yank
                                            (conj keycodes-visual "y"))})
           "Y" (make-operator-current-line yank-range))))

(listen
  :visual-mode-keymap
  (fn [keymap _]
    (let [fn-yank (make-operator set-visual-range yank-range)]
      (assoc keymap "y" visual-keymap-y))))
