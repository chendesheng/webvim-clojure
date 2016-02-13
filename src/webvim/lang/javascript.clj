(ns webvim.lang.javascript
  (:use webvim.core.lang
        webvim.indent))

(println "load javascript language")

(defmethod init-file-type ".js"
  [buf]
  (-> buf
      (assoc-in [:language :id] ::javascript)
      (assoc-in [:language :name] "JavaScript")
      (assoc :tabsize 4)
      (assoc :expandtab false)))

(defmethod indent-pos ::javascript
  [lang r pos]
  (clang-indent r pos))

(defmethod indent-trigger? ::javascript
  [lang keycode]
  (= keycode "}"))
