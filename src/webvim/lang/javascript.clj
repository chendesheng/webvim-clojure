(ns webvim.lang.javascript
  (:use webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.line
        webvim.core.lang
        webvim.indent
        webvim.core.utils))

(println "load javascript language")

(defonce ^:private listen-new-buffer
  (listen
    :load-language
    (fn [buf]
      (if (= (buf :ext) ".js")
        (-> buf
            (assoc-in [:language :id] ::javascript)
            (assoc-in [:language :name] "JavaScript"))
        buf))))

(defmethod indent-pos ::javascript
  [lang r pos]
  (clang-indent r pos))

(defmethod indent-trigger? ::javascript
  [lang keycode]
  (= keycode "}"))
