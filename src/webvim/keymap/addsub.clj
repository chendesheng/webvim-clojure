(ns webvim.keymap.addsub
  (:require [webvim.core.rope :refer [buf-set-pos buf-replace subr re-test]]
            [webvim.core.line :refer [line-start line-end pos-line]]
            [webvim.core.pos :refer [char- pos-re-seq+]]
            [webvim.core.utils :refer [repeat-chars parse-int]]))

(defn- repeat-prefix-value [buf]
  (-> buf :context :repeat-prefix (or "1") parse-int))

(defn- padding-zeroes [news olds]
  (let [nega? (.startsWith olds "-")
        negb? (.startsWith news "-")
        a (if nega? (-> olds count dec) (count olds))
        b (if negb? (-> news count dec) (count news))
        zeroes (repeat-chars (- (max a b) b) \0)]
    (if negb?
      (str "-" zeroes (subs news 1))
      (str zeroes news))))

(defn- inc-dec-number [f]
  (fn [buf keycode]
    (println "inc-dec-number:" keycode)
    (let [repeat-times (repeat-prefix-value buf)
          f (fn [n] (f n repeat-times))
          pos (buf :pos)
          r (buf :str)
          [line-start line-end] (pos-line r pos)
          line (subr r line-start line-end)
          [a b] (first
                  (filter
                    (fn [[_ b]]
                      (-> b
                          (+ line-start)
                          (> pos)))
                    (pos-re-seq+ line 0 #"(?i)(0x[0-9a-fA-F]+)|(-?[0-9]+)")))]
      (if (nil? a)
        buf
        (let [s (str (subr line a b))
              [a b news] (if (re-test s #"^0x|X")
                           [(+ a line-start 2)
                            (+ b line-start)
                            (format (if (re-test s #"[A-Z]") "%X" "%x")
                                   ;http://stackoverflow.com/questions/508630/java-equivalent-of-unsigned-long-long
                                    (-> s (subs 2) (Long/parseUnsignedLong 16) f))]
                           [(+ a line-start)
                            (+ b line-start)
                            (-> s Long/parseLong f str (padding-zeroes s))])]
          
          (-> buf
              (buf-set-pos (dec b))
              (buf-replace a b news)
              char-))))))

(defn wrap-keymap-addsub [keymap]
  (println "addsub normal-mode-keymap")
  (assoc keymap
         "<c-a>" (inc-dec-number +)
         "<c-x>" (inc-dec-number -)))
