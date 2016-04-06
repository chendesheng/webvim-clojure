(ns webvim.keymap.yank
  (:require
    [clojure.string :as string]
    [webvim.core.utils :refer [nop]]
    [webvim.core.rope :refer [buf-set-pos buf-subr]]
    [webvim.core.register :refer [registers-yank-to! registers-put!]]
    [webvim.core.range :refer [range-linewise]]
    [webvim.keymap.compile :refer [wrap-keycode]]
    [webvim.keymap.motion :refer [init-motion-keymap-for-operators]]
    [webvim.keymap.operator :refer [visual-block-lines make-operator
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
  (let [fn-yank (make-operator set-visual-range yank-range)]
    (assoc keymap "y" 
           (fn [buf keycode]
             (if (= (-> buf :visual :type) :visual-block)
               (visual-block-yank buf)
               (fn-yank buf keycode))))))
