(ns webvim.keymap.delete
  (:require 
    [webvim.core.utils :refer [nop]]
    [webvim.core.rope :refer [buf-set-pos buf-delete buf-subr]]
    [webvim.core.line :refer [line-end line-start]]
    [webvim.core.event :refer [listen]]
    [webvim.keymap.compile :refer [wrap-keycode]]
    [webvim.keymap.visual :refer [wrap-temp-visual-mode]]
    [webvim.core.register :refer [registers-delete-to!]]
    [webvim.keymap.yank :refer [yank-blockwise]]
    [webvim.keymap.operator :refer [set-range set-linewise set-line-end
                                    set-visual-range visual-block-lines
                                    make-operator]]
    [webvim.keymap.motion :refer [init-motion-keymap-for-operators]]))

;delete [a b) shift pos
(defn- shift-delete [pos a b]
  (cond
    (and (<= a pos) (< pos b)) a
    (>= pos b) (- pos (- b a))
    :else pos))

(defn- shift-ranges-delete [ranges a b]
  (if (< a b)
    (map 
      (fn [[a1 b1]]
        [(shift-delete a1 a b)
         (shift-delete b1 a b)])
      ranges) ranges))

(defn visual-block-delete [buf]
  (let [items (visual-block-lines buf)
        buf (buf-set-pos buf (-> items last (get 1)))]
    (yank-blockwise buf (rseq items))
    (reduce (fn [buf [_ a b]]
              (-> buf
                  (update-in [:visual :ranges] shift-ranges-delete a b)
                  (buf-delete a b))) buf items)))

(defn delete-range [buf [a b :as rg]]
  (let [s (buf-subr buf a b)]
    (registers-delete-to! (-> buf :context :register)
                          {:str s :linewise? (-> buf :context :linewise?)})
    (-> buf
        (buf-delete a b)
        (update :context dissoc :register))))

(defn- temp-visual-keymap-d [buf keycode]
  (if (-> buf :visual :type (= :visual-block))
    (visual-block-delete buf)
    ((make-operator delete-range) buf keycode)))

(defn wrap-keymap-delete [keymap visual-keymap]
  (let [motion-keymap (init-motion-keymap-for-operators)
        visual-keymap (wrap-temp-visual-mode visual-keymap temp-visual-keymap-d)
        fn-delete (make-operator delete-range)]
    (assoc keymap
           "D" (make-operator set-line-end delete-range)
           "x" fn-delete
           "d" (merge
                 motion-keymap
                 visual-keymap
                 {"d" (wrap-keycode set-linewise)
                  :after (fn [buf keycode]
                           (if (contains? visual-keymap keycode) buf
                               (fn-delete buf keycode)))}))))


(listen
  :visual-mode-keymap
  (fn [keymap _]
    (let [fn-delete (make-operator set-visual-range delete-range)]
      (assoc keymap
             "d" (fn [buf keycode]
                   (if (= (-> buf :visual :type) :visual-block)
                     (visual-block-delete buf)
                     (fn-delete buf keycode)))))))