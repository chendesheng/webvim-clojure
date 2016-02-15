(ns webvim.core.lang
  (:use webvim.core.event))
;different language use differnt indent-pos, word-re etc.

(defmulti indent-pos (fn [lang r pos] (lang :id)))
(defmulti indent-trigger? (fn [lang keycode] (lang :id)))
(defmulti word-re :id)

(defmulti init-file-type :ext)

(defmethod word-re :default [lang]
  (let [word-chars "\\w"
        space-chars "\\s"]
    {:word-chars word-chars
     :not-word-chars (str "^" word-chars)
     :space-chars space-chars
     :not-space-chars (str "^" space-chars)
     :punctuation-chars (str "^" word-chars space-chars)
     :not-punctuation-chars (str word-chars space-chars)}))

(defmethod init-file-type :default
  [buf]
  (-> buf
      (assoc-in [:language :id] ::plain-text)
      (assoc-in [:language :name] "Plain Text")
      (assoc :tabsize 4)
      (assoc :expandtab false)))
