(ns webvim.lang.go
  (:use webvim.core.lang
        webvim.core.event
        webvim.core.rope
        webvim.core.diff
        webvim.core.line
        webvim.core.utils
        clojure.pprint
        webvim.indent))

(println "load go language")

(defmethod init-file-type ".go"
  [buf]
  (-> buf
      (assoc-in [:language :id] ::go)
      (assoc-in [:language :name] "Go")
      (assoc :tabsize 4)
      (assoc :expandtab false)))

(defmethod indent-pos ::go
  [lang r pos]
  (clang-indent r pos))

(defmethod indent-trigger? ::go
  [lang keycode]
  (= keycode "}"))

(defn- format-buffer [buf]
  (if (-> buf :language :id (= ::go))
    (let [res (clojure.java.shell/sh "goimports" "-d" :in (str (buf :str)))]
      (println "gofmt")
      (if (-> res :exit zero? not)
        (assoc buf :message (res :err))
        (-> buf
            (apply-line-changes (parse-diff (str (res :out))))
            save-undo)))
    buf))

(listen :write-buffer
        (fn [buf]
          (format-buffer buf)))
