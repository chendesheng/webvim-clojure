(ns webvim.ui.view.statusbar
  (:require
    [clojure.string :as string]
    [webvim.ui.lib.dom :refer [bounding-rect $id $text-content add-class
                               remove-class toggle-class]]))

(def normal-mode 0)
(def insert-mode 1)
(def ex-mode 2)

(defn- mode-text [mode submode visual-type]
  (let [modes ["NORMAL" "INSERT"]
        submodes ["" "(insert)"];
        visual-types ["" "VISUAL" "VISUAL LINE" "VISUAL BLOCK"]]
    (print "render-mode:" mode submode visual-type)
    (if (< mode (count modes))
      (str "-- "
           (cond
             (and (zero? submode) (zero? visual-type))
             (modes mode)
             (and (zero? submode) (pos? visual-type))
             (visual-types visual-type)
             (pos? submode)
             (str (submodes submode)
                  (if (pos? visual-type)
                    (str " " (visual-types visual-type)))))
           " --"))))

(defn- render-mode [{old-mode :mode
                     old-submode :submode
                     {old-visual-type :type} :visual}
                    {mode :mode
                     submode :submode
                     {visual-type :type} :visual}]
  (if (or (not= old-mode mode)
          (not= old-submode submode)
          (not= old-visual-type visual-type))
    (let [text (mode-text mode submode visual-type)]
      (if-not (empty? text)
        ($text-content ($id "status-bar-buf") text)))))

(defn- render-message [{old-message :message} {message :message}]
  (print "render-message")
  (when (and (not= old-message message)
             (not= message ""))
    ($text-content ($id "status-bar-buf") message)))

(defn- render-status-bar-cursor [$status $cur pos]
  (let [left (if (nil? pos)
               -100
               (.-left (bounding-rect (.-firstChild $status) pos)))]
    (set! (-> $cur .-style .-left) (str (dec left) "px"))))

(defn- render-line-buffer [{old-line-buf :line-buffer} {line-buf :line-buffer}]
  (let [$statusbar ($id "status-bar")]
    (if (and (not= old-line-buf line-buf)
             (some? line-buf))
      (let [$status ($id "status-bar-buf")
            {str :str pos :pos pos2 :pos2} line-buf]
        ($text-content $status str)
        (render-status-bar-cursor $status ($id "status-bar-cursor") pos)
        (render-status-bar-cursor $status ($id "status-bar-cursor-second") pos2)
        (add-class $statusbar "focus"))
      (if $statusbar
        (remove-class $statusbar "focus")))))

(defn- render-name [{old-name :name
                     old-dirty :dirty}
                    {name :name
                     dirty :dirty}]
  (if (not= old-name name)
    ($text-content ($id "status-bar-name") name)
    (if (not= old-dirty dirty)
      (toggle-class ($id "status-bar-name") "buf-dirty" dirty))))

(defn- render-showkeys [{old-showkeys :showkeys} {showkeys :showkeys}]
  (if (not= old-showkeys showkeys)
    (let [$keys ($id "status-bar-keys")]
      ($text-content $keys (string/join "" (reverse showkeys)))
      (if (-> showkeys first nil?)
        (js/setTimeout #($text-content $keys "") 100)))))

(defn render-status-bar [old-buf buf]
  (render-showkeys old-buf buf)
  (render-name old-buf buf)
  (render-line-buffer old-buf buf)
  (render-message old-buf buf)
  (render-mode old-buf buf))

