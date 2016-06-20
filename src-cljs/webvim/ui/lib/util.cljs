(ns webvim.ui.lib.util)

(defn set-prop! [jsobj k v]
  (aset jsobj (name k) v))

(defn set-props! [jsobj obj]
  (doseq [[k v] obj]
    (set-prop! jsobj k v))
  jsobj)

(defn current-path []
  (->> js/window .-location .-href
       (re-seq #"(?i)^http(?:s?://[^/]*)/([^?]*)?")
       last
       last))

(defn current-time []
  (.getTime (js/Date.)))
