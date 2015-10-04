(ns webvim.action.visual
  (:use webvim.core.rope))

(defn visual-mode-select[t keycode]
  (let [m (re-find #"[ocdy]" keycode)] ;don't change cursor position if not motion
    (if (nil? m)
      (let [pos (t :pos)]
        (update-in t [:visual :ranges 0] 
                   (fn[[a b]] [pos b]))) t)))

(defn swap-visual-start-end[buf]
  (let [[a b] (-> buf :visual :ranges first)
        newt (-> buf
                 (assoc-in [:visual :ranges 0] [b a])
                 (buf-set-pos b))]
    (assoc-in newt [:context :lastbuf] newt)))
