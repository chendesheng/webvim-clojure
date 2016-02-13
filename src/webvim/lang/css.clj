(ns webvim.lang.css
  (:use webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.line
        webvim.core.lang
        webvim.indent
        webvim.core.utils))

(defonce ^:private listen-new-buffer
  (listen
    :load-language
    (fn [buf]
      (if (= (buf :ext) ".css")
        (-> buf
            (assoc-in [:language :id] ::css)
            (assoc-in [:language :name] "CSS"))
        buf))))

(defmethod indent-pos ::css
  [lang r pos]
  (clang-indent r pos))

(defmethod indent-trigger? ::css
  [lang keycode]
  (= keycode "}"))

