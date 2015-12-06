(ns webvim.keymap.macro
  (:use webvim.keymap.action))

(defn replay-keys [buf keycodes]
  (let [[buf changes] (apply-keycodes 
                        buf 
                        (-> buf :root-keymap (dissoc "."))
                        keycodes)] 
    (update-in buf [:changes] concat changes))) 

