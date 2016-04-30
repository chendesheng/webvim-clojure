(ns webvim.lang.csharp
  (:use webvim.core.lang
        webvim.indent))

(println "load c# language")

(defmethod init-file-type ".cs"
  [buf]
  (-> buf
      (assoc-in [:language :id] ::csharp)
      (assoc-in [:language :name] "C#")
      (assoc :tabsize 4)
      (assoc :expandtab false)))

(defmethod indent-pos ::csharp
  [buf]
  (clang-indent buf))

(defmethod indent-trigger? ::csharp
  [lang keycode]
  (= keycode "}"))
