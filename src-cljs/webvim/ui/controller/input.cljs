(ns webvim.ui.controller.input
  (:require
    [clojure.string :as string]
    [webvim.ui.lib.dom :refer [$remove $hiccup $text-content $hidden-input add-class remove-class $cursor $bufid get-element-and-offset]]
    [webvim.ui.client :refer [active-buffer]]
    [webvim.ui.lib.event :refer [dispatch-event add-listener add-listener-once]]
    [webvim.ui.lib.keycode :refer [char32bits? KEYCODE-DIC KEYCODE-KEYDOWN special-key? map-shift-char]]))

;Handle user input events, including: keyboard events, resize events
;Generate custom events

(defn- escape-keys [keys]
  (and keys (.replace keys #"([\\<>])" "\\$1")))

(defn- get-caret [$input]
  (.-selectionStart $input))

(defn- insert-node-at-pos [{bufid :id [x y] :cursor} node]
  (let [$lines ($bufid "lines-" bufid)
        $lines-nodes (.-childNodes $lines)
        $line (aget $lines-nodes y)
        [ele idx] (get-element-and-offset $line x)]
    (doto (js/document.createRange)
      (.setStart ele idx)
      (.setEnd ele idx)
      (.insertNode node))))

(defn ime-preview [$input]
  (let [preview-node (atom nil)
        preview-cursor (atom nil)
        remove-preview-node (fn []
                              (when (some? @preview-node)
                                ($remove @preview-node)
                                (reset! preview-node nil)
                                (reset! preview-cursor nil)))
        set-preview-content (fn [text]
                              (if (some? @preview-cursor)
                                ($remove @preview-cursor)
                                (reset! preview-cursor ($hiccup [:span.ime-cursor])))
                              (if (nil? @preview-node)
                                (do
                                  (reset! preview-node ($hiccup [:span.ime-preview text]))
                                  (insert-node-at-pos (active-buffer) @preview-node))
                                ($text-content @preview-node text))
                              (let [pos (get-caret $input)]
                                (doto (js/document.createRange)
                                  (.setStart (.-firstChild @preview-node) pos)
                                  (.setEnd (.-firstChild @preview-node) pos)
                                  (.insertNode @preview-cursor))
                                preview-node))]
    {:on-typing (fn []
                  (let [text (.-value $input)]
                    (if (-> text .-length pos?)
                      (do
                        (set-preview-content text)
                        (-> (active-buffer) :id $cursor (add-class "ime")))
                      (do
                        (remove-preview-node)
                        (-> (active-buffer) :id $cursor (remove-class "ime"))))))
     :on-input (fn []
                 (-> $input .-value (set! ""))
                 (remove-preview-node)
                 (-> (active-buffer) :id $cursor (remove-class "ime")))}))

;buildin keymap
(defn- keymap-alias [key]
  (or ({"<c-[>" "<esc>"
        "<c-h>" "<bs>"
        "<c-i>" "<tab>"
        "<c-m>" "<cr>"} key) key))

(def normal-mode 0)

(defn- current-mode []
  ((active-buffer) :mode))

;custom keymap
(defn- keymap [key f]
  (let [keymaps [{"<c-l>" ":nohl<cr>zz"} {} {}]]
    (f (or ((keymaps (current-mode)) key) key))))

(defn- handle-key [key]
  (println "key:" key)
  (if-not (-> key count zero?)
    (let [key (if (special-key? key)
                (str "<" key ">")
                (escape-keys key))]
      (keymap (keymap-alias key)
              (fn [key]
                (dispatch-event :input-key key))))))

(defn- get-key-for-keydown [event]
  (let [code (.-keyCode event)
        ctrl? (.-ctrlKey event)
        alt? (.-altKey event)
        shift? (.-shiftKey event)
        keyc (KEYCODE-DIC code)]
    (if (not (contains? #{0 16 17 18} code))
      (map-shift-char
        (str (if ctrl? "c-")
             (if alt? "a-")
             (if shift? "s-")
             (escape-keys keyc))))))

(defn- onkeydown [event]
  (let [key (get-key-for-keydown event)]
    (when (special-key? key)
      (.stopPropagation event)
      (.preventDefault event)
      (handle-key key))))

(add-listener-once
  :onload
  (fn []
    (.addEventListener js/document "keydown" (fn [event]
                                               (let [key (get-key-for-keydown event)]
                                                 (when key
                                                   (.stopPropagation event)
                                                   (.preventDefault event)
                                                   (handle-key key)))));
    (let [bufid (:id (active-buffer))
          $input ($hidden-input)
          ime-handler (ime-preview $input)]
      (.addEventListener $input "keydown" (fn [event]
                                            (let [key (get-key-for-keydown event)]
                                              (when (special-key? key)
                                                (.stopPropagation event)
                                                (.preventDefault event)
                                                (handle-key key)))))
      (.addEventListener $input "input" (fn [event]
                                          (js/setTimeout (:on-typing ime-handler) 0)))
      (.addEventListener $input "textInput" (fn [event]
                                              (keymap (-> event .-data escape-keys keymap-alias)
                                                      (fn [key]
                                                        (dispatch-event :input-key key)))
                                              (js/setTimeout (:on-input ime-handler) 0))))))

