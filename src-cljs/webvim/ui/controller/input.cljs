(ns webvim.ui.controller.input
  (:require
    [clojure.string :as string]
    [webvim.ui.lib.dom :refer [$remove $hiccup $text-content $hidden-input add-class remove-class $cursor $bufid get-element-and-offset]]
    [webvim.ui.client :refer [active-buffer]]
    [webvim.ui.lib.event :refer [dispatch-event add-listener add-listener-once]]
    [webvim.ui.lib.keycode :refer [char32bits? KEYCODE-DIC KEYCODE-KEYDOWN special-key?]]))

;Handle user input events, including: keyboard events, resize events
;Generate custom events

(defn- escape-keys [keys]
  (.replace keys #"([\\<>])" "\\$1"))

(defn- get-caret [$input]
  (.-selectionStart $input))

(defn- insert-node-at-pos [{bufid :id [x y] :cursor} node]
  (println "insert-node-at-pos" bufid node x y)
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
                              (let [pos (get-caret $input)
                                    range (js/document.createRange)]
                                (doto range
                                  (.setStart (.-firstChild @preview-node) pos)
                                  (.setEnd (.-firstChild @preview-node) pos)
                                  (.insertNode @preview-cursor))
                                preview-node))]
    {:on-typing (fn []
                  (let [text (.-value $input)]
                    (println "ontyping:" text)
                    (if (-> text .-length pos?)
                      (do
                        (set-preview-content text)
                        (-> (active-buffer) :id $cursor (add-class "ime")))
                      (do
                        (remove-preview-node)
                        (-> (active-buffer) :id $cursor (remove-class "ime"))))))
     :on-input (fn []
                 (println "oninput:" (.-value $input))
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

(defn- map-fullwith-to-halfwidth [text]
  (string/join
    (map (fn [c]
           (or
             ({"！" "!"
               "¥" "$"
               "……" "^"
               "（" "("
               "）" ")"
               "【" "["
               "】" "]"
               "「" "{"
               "」" "}"
               "“" "\""
               "：" ":"
               "《" "<"
               "》" ">"
               "‘" "'"
               "／" "/"
               "？" "?"} c)
             c)) (seq text))))

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

(defn onkeypress [event]
  ;(println "event.keyCode:" (.-keyCode event))
  (if-not (.-defaultPrevented event)
    (handle-key (map-fullwith-to-halfwidth (js/String.fromCharCode (.-keyCode event))))))

(add-listener-once
  :onload
  (fn []
    (.addEventListener js/document "keydown" onkeydown);
    (.addEventListener js/document "keypress" onkeypress)
    (let [bufid (:id (active-buffer))
          $input ($hidden-input)
          ime-handler (ime-preview $input)]
      (.addEventListener $input "keydown" (fn [event]
                                            (.stopPropagation event)
                                            (onkeydown event)))
      (.addEventListener $input "keypress" (fn [event]
                                             (.stopPropagation event)))
      (.addEventListener $input "input" (fn [event]
                                          (println "input:" event)
                                          (js/setTimeout (:on-typing ime-handler) 0)))
      (.addEventListener $input "textInput" (fn [event]
                                              (println "textInput" event)
                                              (keymap (-> event .-data escape-keys keymap-alias)
                                                      (fn [key]
                                                        (dispatch-event :input-key key)))
                                              (js/setTimeout (:on-input ime-handler) 0))))))

