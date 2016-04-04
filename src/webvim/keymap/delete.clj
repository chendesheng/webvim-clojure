(ns webvim.keymap.delete
  (:require 
    [webvim.core.utils :refer [nop]]
    [webvim.core.rope :refer [buf-set-pos buf-delete buf-subr]]
    [webvim.core.line :refer [line-end line-start]]
    [webvim.keymap.compile :refer [wrap-keycode]]
    [webvim.core.register :refer [registers-delete-to!]]
    [webvim.keymap.operator :refer [set-range set-linewise set-line-end
                                    set-visual-range visual-block-delete
                                    make-operator]]
    [webvim.keymap.motion :refer [init-motion-keymap-for-operators]]))

(defn- delete-range [buf [a b :as rg]]
  (let [s (buf-subr buf a b)]
    (registers-delete-to! (-> buf :context :register)
                          {:str s :linewise? (-> buf :context :linewise?)})
    (-> buf
        (buf-delete a b)
        (update :context dissoc :register))))

(defn wrap-keymap-delete [keymap]
  (let [motion-keymap (init-motion-keymap-for-operators)
        fn-delete (make-operator delete-range)]
    (assoc keymap
           "D" (make-operator set-line-end delete-range)
           "x" fn-delete
           "d" (merge
                 motion-keymap
                 {"d" (wrap-keycode set-linewise)
                  :after fn-delete}))))

(defn wrap-keymap-delete-visual [keymap]
  (let [fn-delete (make-operator set-visual-range delete-range)]
    (assoc keymap
           "d" (fn [buf keycode]
                 (if (= (-> buf :visual :type) :visual-block)
                   (visual-block-delete buf)
                   (fn-delete buf keycode))))))
