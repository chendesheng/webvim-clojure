(ns webvim.lang.csharp
  (:use webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.line
        webvim.core.lang
        webvim.indent
        webvim.core.utils))

(println "load C# language")

(defonce ^:private listen-new-buffer
  (listen
    :load-language
    (fn [buf]
      (if (= (buf :ext) ".cs")
        (-> buf
            (assoc-in [:language :id] ::csharp)
            (assoc-in [:language :name] "C#")
            (assoc :tabsize 4)
            (assoc :expandtab false))
        buf))))

(defmethod indent-pos ::csharp
  [lang r pos]
  (clang-indent r pos))

(defmethod indent-trigger? ::csharp
  [lang keycode]
  (= keycode "}"))
