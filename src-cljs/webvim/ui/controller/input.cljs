(ns webvim.ui.controller.input
  (:require 
    [webvim.ui.client :refer [active-buffer]]
    [webvim.ui.lib.dom :refer [$hidden-input]]
    [webvim.ui.lib.event :refer [dispatch-event add-listener add-listener-once]]
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

(defn- current-mode []
  ((active-buffer) :mode))

;custom keymap
(defn- keymap [key f]
  (let [keymaps [{"<c-l>" ":nohl<cr>"} {} {}]]
    (f (or ((keymaps (current-mode)) key) key))))

(defn- handle-key [key]
  (if-not (-> key count zero?)
    (let [key (if (special-key? key)
                (str "<" key ">")
                (escape-keys key))]
      (keymap (keymap-alias key)
              (fn [key]
                (dispatch-event :input-key key))))))

(defn- onkeydown [event]
  (let [code (.-keyCode event)
        ctrl? (.-ctrlKey event)
        alt? (.-altKey event)
        shift? (.-shiftKey event)
        keyc (KEYCODE-DIC code)
        keyd (KEYCODE-KEYDOWN code)
        key (if (or keyd ctrl? alt?)
              (cond
                ctrl?
                (if (and (not (contains? #{0 16 17 18} code))
                         keyc)
                  (str "c-" (if shift? "s-") (if alt? "a-") keyc)
                  keyd)
                shift?
                (if keyc
                  (str "s-" keyc) keyd)
                :else
                keyd) keyd)]
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
          (handle-key (js/String.fromCharCode (.-keyCode event))))))))
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
