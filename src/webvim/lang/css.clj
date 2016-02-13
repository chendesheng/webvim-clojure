(ns webvim.lang.css
  (:use webvim.core.lang
        webvim.indent))

(defmethod init-file-type ".css"
  [buf]
  (-> buf
      (assoc-in [:language :id] ::css)
      (assoc-in [:language :name] "CSS")
      (assoc :tabsize 4)
      (assoc :expandtab false)))

(defmethod indent-pos ::css
  [lang r pos]
  (clang-indent r pos))

(defmethod indent-trigger? ::css
  [lang keycode]
  (= keycode "}"))

