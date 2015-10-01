(ns webvim.lang.default
  (:use webvim.core.event
        webvim.core.lang))

(defmethod word-re :default [lang]
  (let [word-chars "\\w"
        space-chars "\\s"]
    {:word-chars word-chars
     :not-word-chars (str "^" word-chars)
     :space-chars space-chars
     :not-space-chars (str "^" space-chars)
     :punctuation-chars (str "^" word-chars space-chars)
     :not-punctuation-chars (str word-chars space-chars) }))

