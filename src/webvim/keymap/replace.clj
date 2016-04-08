(ns webvim.keymap.replace
  (:require [webvim.core.utils :refer [keycode-to-char nop]]
            [webvim.core.rope :refer [buf-set-pos subr buf-replace]]
            [webvim.core.event :refer [listen]]
            [webvim.indent :refer [buf-indent-current-line]]
            [webvim.keymap.operator :refer [not-empty-range range-prefix]]))

(defn- replace-char [buf a b ch]
  (buf-replace buf a b
               (-> buf :str (subr a b) str
                   (.replaceAll "[^\r\n]" ch))))

(defmulti replace-char-keycode
  (fn [buf keycode]
    (if (not= (count (keycode-to-char keycode)) 1)
      :nop
      (cond
        (= :no-visual (-> buf :visual :type))
        :no-visual
        (= :visual-block (-> buf :visual :type))
        :visual-block
        :else
        :not-visual-block))))

(defmethod replace-char-keycode :nop [buf keycode]
  buf)

(defmethod replace-char-keycode :not-visual-block [buf keycode]
  (let [ch (keycode-to-char keycode)
        r (buf :str)
        pos (buf :pos)
        [a b :as rg] (range-prefix buf true)]
    (-> buf
        (replace-char a b ch) 
        (buf-set-pos a))))

(defmethod replace-char-keycode :visual-block [buf keycode]
  (let [ranges (-> buf :visual :ranges)
        firstline (last ranges) ;ranges in reverse order
        ch (keycode-to-char keycode)
        r (buf :str)
        pos (buf :pos)
        _ (println "ranges:" (not-empty-range ranges))
        newbuf (reduce
                 (fn [buf [a b]]
                   (replace-char buf a (inc b) ch)) buf (not-empty-range ranges))]
    (buf-set-pos newbuf (first firstline))))

(defmethod replace-char-keycode :no-visual [buf keycode]
  (let [ch (keycode-to-char keycode)
        pos (buf :pos)]
    (cond
      (= ch "\n")
      (-> buf
          (buf-replace pos (inc pos) ch)
          (buf-set-pos (inc pos))
          buf-indent-current-line)
      (= (count ch) 1)
      (-> buf
          (buf-replace pos (inc pos) ch)
          (buf-set-pos pos))
      :else buf)))

(defn wrap-keymap-replace [keymap]
  (-> keymap
      (assoc "r" {"<esc>" nop
                  :else replace-char-keycode})))

(listen
  :visual-mode-keymap
  (fn [keymap _]
    (-> keymap
        (assoc "r" {"<esc>" nop
                    "<cr>" nop
                    :else replace-char-keycode}))))
