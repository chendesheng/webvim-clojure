(ns webvim.keymap.macro
  (:use webvim.keymap.action))

(defn replay-keys [buf keycodes]
  (let [keys (buf :keys)
        [buf changes] (apply-keycodes (dissoc buf :keys) keycodes)] 
    (-> buf
        (update-in [:changes] concat changes)
        (assoc :keys keys))))

