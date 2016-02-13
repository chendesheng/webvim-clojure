(ns webvim.lang.go
  (:use webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.line
        webvim.core.lang
        webvim.indent
        webvim.core.utils))

(println "load go language")

(defonce ^:private listen-new-buffer
  (listen
    :load-language
    (fn [buf]
      (if (= (buf :ext) ".go")
        (-> buf
            (assoc-in [:language :id] ::go)
            (assoc-in [:language :name] "Go")
            (assoc :tabsize 4)
            (assoc :expandtab false))
        buf))))

(defmethod indent-pos ::go
  [lang r pos]
  (clang-indent r pos))

(defmethod indent-trigger? ::go
  [lang keycode]
  (= keycode "}"))
