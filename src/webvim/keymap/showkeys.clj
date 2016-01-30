(ns webvim.keymap.showkeys
  (:use clojure.pprint
        webvim.keymap.action
        webvim.core.ui
        webvim.core.buffer
        webvim.core.event))

(defn- on-before-handle-key[buf keycode]
  (println "on-before-handle-key:" keycode)
  (update-in buf [:showkeys]
             (fn[showkeys]
                 (if (and (= (buf :mode) normal-mode)
                          (-> buf :line-buffer nil?)
                          (not= keycode "/")
                          (not= keycode ":"))
                   (conj showkeys keycode)
                   nil))))

(defn- on-normal-mode-keymap[keymap]
  (key-do-after 
    keymap :after
    (fn[buf keycode]
      (cond (= "\"" keycode)
        buf
        (= "<esc>" keycode)
        (assoc buf :showkeys nil)
        :else
        (do
          (send ui-agent
                (fn[ui]
                  (update-in ui [:buf] dissoc :showkeys)))
          (send (@buffer-list (buf :id))
                (fn[buf]
                  (dissoc buf :showkeys)))
          (update-in buf [:showkeys] conj nil))))))

(defonce ^:private listener1
  (listen
    :before-handle-key
    (fn[buf keycode]
      (on-before-handle-key buf keycode))))

(defonce ^:private listener2
  (listen
    :normal-mode-keymap
    (fn[keymap]
      (on-normal-mode-keymap keymap))))

