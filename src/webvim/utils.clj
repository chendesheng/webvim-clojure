(ns webvim.utils)

(defn sort2[a b]
  (if (< a b) [a b] [b a]))

(def all-braces {\( \) \) \( \[ \] \] \[ \{ \} \} \{})
(def left-braces #{\( \[ \{})
(def right-braces #{\) \] \}})

