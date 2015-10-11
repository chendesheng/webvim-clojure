(ns webvim.utils)

(def all-braces {\( \) \) \( \[ \] \] \[ \{ \} \} \{})
(def left-braces #{\( \[ \{})
(def right-braces #{\) \] \}})

(defn quote-pattern[ch]
  (java.util.regex.Pattern/quote (str ch)))

(defn sort2
  ([a b]
   (if (< a b) [a b] [b a]))
  ([[a b]]
   (sort2 a b)))

(defn keycode-to-char[keycode]
  (cond 
    (= 1 (count keycode))
    keycode
    (= "<cr>" keycode)
    "\n"
    (= "<tab>" keycode)
    "\t"
    (= "<space>" keycode)
    " "
    :else ""))

(defn make-range
  ([a b inclusive?]
   (if inclusive? 
     (let [[a b] (sort2 a b)]
       [a (inc b)])
     (sort2 a b)))
  ([[a b] inclusive?]
   (make-range a b inclusive?)))

(defn bound-range[v r e]
  (cond (<= v r) r
        (>= v e) e
        :else v))
