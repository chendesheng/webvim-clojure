(ns webvim.core.lang
  (:use webvim.core.event))
;different language use differnt indent-pos, word-re etc.

(defmulti indent-pos (fn[lang r pos] (lang :id)))
(defmulti indent-trigger? (fn[lang keycode] (lang :id)))
(defmulti word-re :id)
