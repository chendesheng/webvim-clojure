(ns webvim.lang.sql
  (:use webvim.core.lang))

(println "load sql language")

(defmethod init-file-type ".sql"
  [buf]
  (-> buf
      (assoc-in [:language :id] ::sql)
      (assoc-in [:language :name] "SQL")
      (assoc :tabsize 4)
      (assoc :expandtab false)))
