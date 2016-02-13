(ns webvim.lang.sql
  (:use webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.line
        webvim.core.lang
        webvim.indent
        webvim.core.utils))

(println "load sql language")

(defonce ^:private listen-new-buffer
  (listen
    :load-language
    (fn [buf]
      (if (= (buf :ext) ".sql")
        (-> buf
            (assoc-in [:language :id] ::sql)
            (assoc-in [:language :name] "SQL")
            (assoc :tabsize 4)
            (assoc :expandtab true))
        buf))))

