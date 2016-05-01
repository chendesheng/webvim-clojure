(ns webvim.keymap.visual
  (:require [clojure.string :as str]
            [webvim.keymap.operator :refer [set-visual-ranges set-default-inclusive set-linewise]]
            [webvim.keymap.motion :refer [init-motion-keymap-with-objects init-motion-keymap-fix-cw]]
            [webvim.keymap.compile :refer [wrap-key]]
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

(defn- clear-visual [buf _]
  (-> buf
      (assoc :last-visual (-> buf :visual (dissoc :ranges))) ;keep last visual
      (assoc :visual {:type :no-visual :range [0 0]})))

(defn- swap-visual-start-end [buf keycode]
  (let [[a b] (-> buf :visual :range)]
    (-> buf
        (assoc-in [:visual :range] [b a])
        (buf-set-pos b))))

(def keycodes-visual #{"v" "V" "<c-v>"})
(defn visual-keycode? [keycode]
  (contains? keycodes-visual keycode))
(defn- keycode2type [keycode]
  ({"v" :visual-range "V" :visual-line "<c-v>" :visual-block} keycode))
(defn- visual-keycodes-map [f]
  (reduce #(assoc %1 %2 f) {} keycodes-visual))

;type/mode     | keycode | next
;----------- --|---------|-------
;normal        |  v      | :visual-range
;normal        |  V      | :visual-line
;:visual-range |  V      | :visual-line
;:visual-range |  v      | normal
;:visual-line  |  v      | :visual-range
;:visual-line  |  V      | normal
(defn- visual-mode-continue? [buf keycode]
  (if (-> buf :context :cancel-visual-mode?)
    false
    (let [typ (-> buf :context :last-visual-type)
          newtyp (keycode2type keycode)]
      (if (nil? newtyp)
        (not (contains? #{"A" "I" "d" "c" "y" "=" "u" "<esc>" "<" ">" "r"} keycode))
        (not (= typ newtyp))))))

(defn- change-visual-mode-type [buf keycode]
  (log "change-visual-mode-type")
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
            (contains? #{"j" "k"} keycode))
      buf
      (assoc buf :x (column buf)))))

(defn- set-visual-mode [buf visual]
  (-> buf
      (assoc :visual visual)
      set-visual-ranges))

(defn- init-visual-mode-keymap [motion-keymap]
  (-> motion-keymap 
      (assoc 
        :enter (fn [buf keycode]
                 (let [pos (buf :pos)]
                   (set-visual-mode buf 
                                    {:type (keycode2type keycode)
                                     :range [pos pos]})))
        :leave clear-visual
        :continue visual-mode-continue?
        :before (fn [buf keycode] 
                  (update buf :context
                          (fn [context]
                            (-> context
                                (assoc :last-visual-type (-> buf :visual :type)
                                       :cancel-visual-mode? false)))))
        :after (fn [buf keycode]
                 (log ["visual after:" keycode (-> buf :context :range)])
                 (-> buf
                     (update-in [:visual :range]
                                (fn [rg]
                                  (or (-> buf :context :range)
                                      (assoc rg 0 (buf :pos)))))
                     set-visual-ranges
                     (update :context dissoc :range)
                     (update-x-if-not-jk keycode))))
      (merge (visual-keycodes-map change-visual-mode-type))))

(defn init-visual-mode-keymap-for-operators []
  (let [motion-keymap (init-motion-keymap-fix-cw)]
    (-> motion-keymap
        init-visual-mode-keymap
        (assoc :continue (fn [_ keycode]
                           (visual-keycode? keycode))))))

(defn- init-visual-mode-keymap-with-operators [motion-keymap buf]
  (fire-event :visual-mode-keymap
              (-> motion-keymap 
                  init-visual-mode-keymap
                  (assoc "o" swap-visual-start-end
                         "<tab>" nop
                         "<c-o>" nop
                         "<c-r>" nop)
                  wrap-keymap-scrolling-visual) buf))

(defn wrap-keymap-visual [keymap buf]
  (let [motion-keymap (init-motion-keymap-with-objects)
        visual-mode-keymap (init-visual-mode-keymap-with-operators motion-keymap buf)]
    (-> keymap
        (merge (visual-keycodes-map visual-mode-keymap))
        (update "g"
                assoc "v" (assoc
                            visual-mode-keymap
                            :enter
                            (fn [buf keycode]
                              (let [visual (buf :last-visual)]
                                (-> buf
                                    (set-visual-mode visual)
                                    (buf-set-pos (-> visual :range first))))))))))

(defn- set-temp-visual-mode-range [{r :str {rg :range typ :type} :visual :as buf} keycode]
  (log typ)
  (-> buf
      (set-linewise (= typ :visual-line))
      (set-default-inclusive keycode)
      (update-in [:context :inclusive?] not)))

;for cv{motion}, dv{motion} etc.
(defn wrap-temp-visual-mode [visual-keymap f]
  (let [visual-keymap (-> visual-keymap
;                          (assoc :enter (fn [buf keycode]
;                                          (assoc buf :visual {:type (keycode2type keycode)
;                                                              :range [0 0]})))
                          (wrap-key :leave
                                    (fn [handler]
                                      (fn [buf keycode]
                                        (comment
                                          (log keycode)
                                          (log (-> buf
                                                   (set-default-inclusive keycode)
                                                   (update-in [:context :inclusive?] not)
                                                   :context :inclusive?)))
                                        (-> buf
                                            (set-temp-visual-mode-range keycode)
                                            (f keycode)
                                            (handler keycode))))))]
    (visual-keycodes-map visual-keymap)))

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
