(ns webvim.keymap.put
  (:require
    [clojure.string :as str]
    [webvim.core.rope :refer [char-at buf-insert buf-set-pos]]
    [webvim.core.line :refer [vertical-line-pos line-start pos-line]]
    [webvim.core.pos :refer [char+]]
    [webvim.core.utils :refer [nop]]
    [webvim.keymap.compile :refer [wrap-keycode]]
    [webvim.core.register :refer [registers-get]]))

(defn- put-blockwise [buf s append?]
  (let [{r :str pos :pos tabsize :tabsize} buf
        lines (str/split-lines s)
        h (count lines)
        newpos (if append? (-> lines first count (+ pos)) pos)
        buf (reduce (fn [buf [pos r]]
                      (if (neg? pos) buf
                          (buf-insert buf pos r)))
                    buf
                    (reverse (map
                               vector
                               (vertical-line-pos buf (if append? (inc pos) pos) h tabsize true)
                               lines)))]
    (buf-set-pos buf newpos)))

(defn- put-linewise [buf s append?]
  (let [[a b] (pos-line buf)
        newpos (if append? b a)]
    (-> buf
        (buf-insert newpos s)
        (buf-set-pos newpos)
        line-start)))

(defn- put-from-register [buf reg append?]
  (let [{s :str res :result linewise? :linewise? blockwise? :blockwise?} (registers-get reg)]
    (cond
      linewise?
      (put-linewise buf s append?)
      blockwise?
      (put-blockwise buf s append?)
      :else
      (let [pos (if append? (inc (buf :pos)) (buf :pos))
            s (if (= reg "=") res s) 
            newpos (-> pos (+ (count s)) dec)]
        (-> buf
            (buf-insert pos s)
            (buf-set-pos newpos))))))


(defn wrap-keymap-put-insert [keymap]
  (assoc keymap
         "<c-r>" {"<esc>" nop
                  :else (fn [buf keycode]
                          (-> buf
                              (put-from-register keycode false)
                              char+))}))

(defn wrap-keymap-put [keymap]
  (assoc keymap
         "p" (fn [buf keycode]
               (let [append? (if (-> buf :context :register registers-get :linewise?)
                               true 
                               (not= (char-at (buf :str) (buf :pos)) \newline))]
               ;(println "append?" append?)
                 (put-from-register buf (-> buf :context :register) append?)))
         "P" (wrap-keycode #(put-from-register % (-> % :context :register) false))))
