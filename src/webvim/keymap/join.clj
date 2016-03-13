(ns webvim.keymap.join
  (:require
    [webvim.core.rope :refer [char-at buf-set-pos buf-replace]]
    [webvim.core.pos :refer [pos-re+]]))

(defn- join-line
  "join current line and next line"
  [buf keycode]
  (let [pos (buf :pos)
        r (buf :str)
        [a b] (pos-re+ r pos #"\r?\n.*?(?=(\r|\n|\S))")]
    (if (nil? a) buf
        (let [sep (if (or (>= b (count r))
                          (= (char-at r b) \))) "" " ")]
          (-> buf
              (buf-replace a b sep)
              (buf-set-pos a))))))

(defn wrap-keymap-join [keymap]
  (assoc keymap "J" join-line))

;TODO: visual mode join
;(defn wrap-keymap-join-visual[keymap]
;  (assoc keymap "J" join-line))

