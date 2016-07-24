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

;http://dev.clojure.org/jira/browse/CLJ-1468
(defn deep-merge
  "Like merge, but merges maps recursively."
  [& maps]
  (if (every? map? maps)
    (apply merge-with deep-merge maps)
    (last maps)))

