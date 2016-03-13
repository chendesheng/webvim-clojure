(ns webvim.keymap.delete
  (:require 
    [webvim.core.utils :refer [nop]]
    [webvim.core.rope :refer [buf-set-pos buf-delete]]
    [webvim.core.line :refer [line-end line-start]]
    [webvim.keymap.compile :refer [wrap-keycode]]
    [webvim.visual :refer [visual-range visual-line visual-block]]
    [webvim.keymap.operator :refer [buf-yank setup-range-line-end setup-range-line setup-range inclusive? visual-block-delete delete-char range-prefix]]
    [webvim.keymap.motion :refer [init-motion-keymap-for-operators]]))

(defn- delete-range [buf inclusive? linewise?]
  (let [[a b] (range-prefix buf inclusive?)]
    ;(println "delete-range:" a b)
    (-> buf
        (buf-yank a b linewise? true)
        (buf-delete a b)
        (buf-set-pos a))))

(defn- delete-to-line-end [buf keycode]
  (-> buf
      setup-range-line-end
      (delete-range false false)))

(defn- delete [buf keycode]
  (if (contains? #{"d" "j" "k"} keycode)
    (-> buf
        setup-range-line
        (delete-range false true)
        line-start)
    (-> buf
        setup-range
        (delete-range (inclusive? keycode) false))))

(defmulti visual-keymap-d (fn [buf keycode] (-> buf :visual :type)))
(defmethod visual-keymap-d visual-range [buf keycode]
  (delete-range buf true false))
(defmethod visual-keymap-d visual-line [buf keycode]
  (delete-range buf true true))
(defmethod visual-keymap-d visual-block [buf keycode]
  (visual-block-delete buf))

(defn wrap-keymap-delete [keymap]
  (let [motion-keymap (init-motion-keymap-for-operators)]
    (assoc keymap
           "D" delete-to-line-end
           "x" (wrap-keycode delete-char)
           "d" (merge
                 motion-keymap
                 {"d" nop
                  :after delete}))))

(defn wrap-keymap-delete-visual [keymap]
  (assoc keymap
         "d" visual-keymap-d))
