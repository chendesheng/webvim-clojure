(ns webvim.ui.keyboard
  (:require [webvim.ui.lib.dom :refer [$hidden-input]]
            [webvim.ui.event :refer [dispatch-event add-listener add-listener-once]]
            [webvim.ui.lib.keycode :refer [char32bits? KEYCODE-DIC KEYCODE-KEYDOWN special-key?]]))

;Handle user input events, including: keyboard events, resize events
;Generate custom events

(defn- escape-keys [keys]
  (.replace keys #"([\\<>])" "\\$1"))

;buildin keymap
(defn- keymap-alias [key]
  (or ({"<c-[>" "<esc>"
        "<c-h>" "<bs>"
        "<c-i>" "<tab>"
        "<c-m>" "<cr>"} key) key))

;custom keymap
(defn- keymap [key f]
  (f key))

(defn- handle-key [key]
  (if-not (-> key count zero?)
    (let [key (if (special-key? key)
                (str "<" key ">")
                (escape-keys key))]
      (keymap (keymap-alias key)
              (fn [key]
                (dispatch-event :input-key key))))))

(defn- onkeydown [event]
  (let [code (-> event .-keyCode)
        ctrl? (-> event .-ctrlKey nil? not)
        alt? (-> event .-altKey nil? not)
        shift? (-> event .-shiftKey nil? not)
        keyc (KEYCODE-DIC code)
        key (or (if keyc
                  (str (if (and ctrl? (not (#{0 16 17 18} code))) "c-")
                       (if shift? "s-")
                       (if alt? "a-")
                       keyc))
                (KEYCODE-KEYDOWN code))]
    (if key
      (do
        (.preventDefault event)
        (handle-key key)))))

(add-listener-once
  :onload
  (fn []
    (.addEventListener js/document "keydown" onkeydown);
    (.addEventListener
      js/document "keypress"
      (fn [event] 
        (if-not (.-defaultPrevented event)
          (handle-key (js/String.fromCharCode event.keyCode)))))))
(comment let [input ($hidden-input)]
         (set! js/document.body.onclick (.-focus input))
         (on-buffer-change
           :mode
           (fn [buf]
             (case (buf :mode)
               :normal
               (do
                 (.blur input)
                 (-> input .-disabled (set! true)))
               :insert
               (do
                 (-> input .-disabled (set! false))
                 (.focus input)))))
         (.addEventListener
           input "keydown"
           (fn [event]
             (.stopPropagation event)
             (onkeydown event))))
