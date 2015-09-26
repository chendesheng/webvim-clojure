(ns webvim.utils)

(def all-braces {\( \) \) \( \[ \] \] \[ \{ \} \} \{})
(def left-braces #{\( \[ \{})
(def right-braces #{\) \] \}})

(defn quote-pattern[ch]
  (java.util.regex.Pattern/quote (str ch)))

(defn sort2[a b]
  (if (< a b) [a b] [b a]))
