(ns webvim.core.lang
  (:use webvim.core.event))
;different language use differnt indent-pos, word-re etc.

(defmulti indent-pos (fn[lang r pos] (lang :id)))
(defmulti indent-trigger? (fn[lang keycode] (lang :id)))
(defmulti word-re :id)

(defonce ^:private listen-new-buffer
  (listen
    :load-language
    (fn [{ext :ext :as buf}]
      (cond 
      	(or (= ext ".html") (= ext ".xml"))
        (-> buf
            (assoc-in [:language :id] ::xml)
            (assoc-in [:language :name] "XML")
            (assoc :tabsize 4)
            (assoc :expandtab false))
        :else
        buf))))
