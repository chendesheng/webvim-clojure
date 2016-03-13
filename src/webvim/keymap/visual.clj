(ns webvim.keymap.visual
  (:require [clojure.string :as str]
            [webvim.keymap.indent :refer [wrap-keymap-indent-visual]]
            [webvim.keymap.case :refer [wrap-keymap-case-visual]]
            [webvim.keymap.replace :refer [wrap-keymap-replace-visual]]
            [webvim.keymap.yank :refer [wrap-keymap-yank-visual]]
            [webvim.keymap.delete :refer [wrap-keymap-delete-visual]]
            [webvim.keymap.change :refer [wrap-keymap-change-visual]]
            [webvim.keymap.operator :refer [set-visual-ranges]]
            [webvim.keymap.motion :refer [init-motion-keymap-with-objects]]
            [webvim.keymap.scrolling :refer [wrap-keymap-scrolling-visual]])
  (:use webvim.keymap.insert
        webvim.keymap.ex
        webvim.core.buffer
        webvim.core.rope
        webvim.core.line
        webvim.core.pos
        webvim.core.register
        webvim.core.event
        webvim.indent
        webvim.core.utils
        webvim.jumplist
        webvim.autocompl))

(defn- not-empty-range [ranges]
  (filter (fn [[a b]]
            (< a (inc b))) ranges))

(defn- clear-visual [buf]
  (-> buf
      (assoc :last-visual (-> buf :visual (dissoc :ranges))) ;keep last visual
      (assoc :visual {:type :no-visual :range [0 0]})))

(defn- visual-select [buf]
  (let [[a b :as rg] (-> buf :context :range)]
    (if (nil? rg)
      (assoc-in buf [:visual :range 0] (buf :pos))
      (-> buf
          (assoc-in [:visual :range] [b a])
          (buf-set-pos b)))))

(defn- swap-visual-start-end [buf keycode]
  (let [[a b] (-> buf :visual :range)]
    (-> buf
        (assoc-in [:visual :range] [b a])
        (buf-set-pos b))))

;type/mode    | keycode | next
;-------------|---------|-------
;normal       |  v      | :visual-range
;normal       |  V      | :visual-line
;:visual-range |  V      | :visual-line
;:visual-range |  v      | normal
;:visual-line  |  v      | :visual-range
;:visual-line  |  V      | normal
(defn- keycode2type [keycode]
  ({"v" :visual-range "V" :visual-line "<c-v>" :visual-block} keycode))

(defn- visual-mode-continue? [buf keycode]
  (if (-> buf :context :cancel-visual-mode?)
    false
    (let [typ (-> buf :context :last-visual-type)
          newtyp (keycode2type keycode)]
      (if (nil? newtyp)
        (not (contains? #{"A" "I" "d" "c" "y" "=" "u" "<esc>" "<" ">" "r"} keycode))
        (not (= typ newtyp))))))

(defn- change-visual-mode-type [buf keycode]
  (let [typ (-> buf :context :last-visual-type)
        newtyp (keycode2type keycode)]
    (if (= typ newtyp) buf
        (-> buf
            (assoc-in [:visual :type] newtyp)
            set-visual-ranges))))

(defn update-x-if-not-jk
  "update :x unless it is up down motion"
  [buf keycode]
  (let [lastbuf (buf :context :lastbuf)]
    (if (or (= (:pos lastbuf) (:pos buf))
            (contains? #{"j" "k" "<c-d>" "<c-u>"} keycode))
      buf
      (assoc buf :x (column buf)))))

(defn- set-visual-mode [buf visual]
  (-> buf
      (assoc :visual visual)
      set-visual-ranges))

(defn- init-visual-mode-keymap [motion-keymap]
  (assoc 
    motion-keymap 
    :enter (fn [buf keycode]
             (let [pos (buf :pos)]
               (set-visual-mode buf 
                                {:type (keycode2type keycode)
                                 :range [pos pos]})))
    :leave (fn [buf keycode] (clear-visual buf))
    :continue visual-mode-continue?
    :before (fn [buf keycode] 
              (update-in buf [:context]
                         (fn [context]
                           (-> context
                               (assoc :last-visual-type (-> buf :visual :type)
                                      :cancel-visual-mode? false)
                               (dissoc :range)))))
    :after (fn [buf keycode]
             (-> buf
                 visual-select
                 set-visual-ranges
                 (update-x-if-not-jk keycode)))
    "V" change-visual-mode-type
    "v" change-visual-mode-type
    "<c-v>" change-visual-mode-type))

(defn init-visual-mode-keymap-for-operators [motion-keymap]
  (let [keymap (-> motion-keymap
                   init-visual-mode-keymap
                   (assoc :continue (constantly false)))]
    {"v" keymap
     "V" keymap
     "<c-v>" keymap}))

(defn- init-visual-mode-keymap-with-operators [motion-keymap buf]
  (fire-event :visual-mode-keymap
              (-> motion-keymap 
                  init-visual-mode-keymap
                  (assoc "o" swap-visual-start-end
                         "<c-i>" nop
                         "<c-o>" nop
                         "<c-r>" nop)
                  wrap-keymap-indent-visual
                  wrap-keymap-replace-visual
                  wrap-keymap-scrolling-visual
                  wrap-keymap-yank-visual
                  wrap-keymap-delete-visual
                  wrap-keymap-case-visual
                  wrap-keymap-change-visual) buf))

(defn wrap-keymap-visual [keymap buf]
  (let [motion-keymap (init-motion-keymap-with-objects)
        visual-mode-keymap (init-visual-mode-keymap-with-operators motion-keymap buf)]
    (-> keymap
        (assoc "v" visual-mode-keymap
               "V" visual-mode-keymap
               "<c-v>" visual-mode-keymap)
        (update-in ["g"]
                   assoc "v" (assoc
                               visual-mode-keymap
                               :enter
                               (fn [buf keycode]
                                 (let [visual (buf :last-visual)]
                                   (-> buf
                                       (set-visual-mode visual)
                                       (buf-set-pos (-> visual :range first))))))))))

;keep track visual ranges when buffer changed
(listen
  :change-buffer
  (fn [buf _ c]
    (let [cpos (c :pos)
          delta (- (-> c :to count) (c :len))]
      (if (nil? (buf :last-visual)) buf
          (update-in buf [:last-visual :range]
                     (fn [[a b :as rg]]
                       [(if (< a cpos) a (+ a delta))
                        (if (< b cpos) b (+ b delta))]))))))
