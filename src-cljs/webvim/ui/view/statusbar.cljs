(ns webvim.ui.view.statusbar
  (:require
    [clojure.string :as string]
    [webvim.ui.lib.dom :refer [bounding-rect $id $text-content add-class
                               $status-bar $status-bar-buf $status-bar-name $status-bar-keys
                               $status-bar-cursor $status-bar-cursor-second
                               remove-class toggle-class]]))

(def normal-mode 0)
(def insert-mode 1)
(def ex-mode 2)

(defn- mode-text [mode submode visual-type]
  (let [submodes ["" "(insert)"];
        visual-types ["" "VISUAL" "VISUAL LINE" "VISUAL BLOCK"]
        text (cond
               (= mode insert-mode)
               "INSERT"
               (pos? submode)
               (str (submodes submode)
                    (if (pos? visual-type)
                      (str " " (visual-types visual-type))))
               (pos? visual-type)
               (visual-types visual-type))]
    (if (empty? text)
      ""
      (str "-- " text " --"))))

(defn- render-mode [{old-mode :mode
                     old-submode :submode
                     {old-visual-type :type} :visual}
                    {mode :mode
                     submode :submode
                     {visual-type :type} :visual
                     bufid :id}]
  (if (or (not= old-mode mode)
          (not= old-submode submode)
          (not= old-visual-type visual-type))
    (let [text (mode-text mode submode visual-type)]
      ($text-content ($status-bar-buf bufid) text))))

(defn- render-message [{old-message :message} {message :message bufid :id}]
  (when (and (not= old-message message)
             (not= message ""))
    ($text-content ($status-bar-buf bufid) message)))

(defn- render-status-bar-cursor [$status $cur pos]
  (let [left (if (nil? pos)
               -100
               (.-left (bounding-rect (.-firstChild $status) pos)))]
    (set! (-> $cur .-style .-left) (str (dec left) "px"))))

(defn- render-line-buffer [{old-line-buf :line-buffer} {line-buf :line-buffer bufid :id}]
  (let [$statusbar ($status-bar bufid)
        $status ($status-bar-buf bufid)]
    (toggle-class $statusbar "focus" (some? line-buf))
    (if (not= old-line-buf line-buf)
      (let [{str :str pos :pos pos2 :pos2} line-buf]
        ($text-content $status str)
        (render-status-bar-cursor $status ($status-bar-cursor bufid) pos)
        (render-status-bar-cursor $status ($status-bar-cursor-second bufid) pos2)))))

(defn- render-name [{old-name :name
                     old-dirty :dirty}
                    {name :name
                     dirty :dirty
                     bufid :id}]
  (if (not= old-name name)
    ($text-content ($status-bar-name bufid) name)
    (if (not= old-dirty dirty)
      (toggle-class ($status-bar-name bufid) "buf-dirty" dirty))))

(defn- render-showkeys [{old-showkeys :showkeys} {showkeys :showkeys bufid :id}]
  (if (not= old-showkeys showkeys)
    (let [$keys ($status-bar-keys bufid)]
      ($text-content $keys (string/join "" (reverse showkeys)))
      (if (-> showkeys first nil?)
        (js/setTimeout #($text-content $keys "") 100)))))

(defn render-status-bar [old-buf buf]
  (render-showkeys old-buf buf)
  (render-name old-buf buf)
  (render-mode old-buf buf)
  (render-line-buffer old-buf buf)
  (render-message old-buf buf))

