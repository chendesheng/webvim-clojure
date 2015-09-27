(ns webvim.action.visual)

(defn visual-mode-select[t keycode]
  (let [m (re-find #"[ocdy]" keycode)] ;don't change cursor position if not motion
    (if (nil? m)
      (let [pos (t :pos)]
        (update-in t [:visual :ranges 0] 
                   (fn[[a b]] [pos b]))) t)))

