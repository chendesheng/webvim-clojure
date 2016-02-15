(ns webvim.lang.html
  (:use webvim.core.lang))

(println "load html language")

(defn- init [buf]
  (-> buf
      (assoc-in [:language :id] ::xml)
      (assoc-in [:language :name] "XML")
      (assoc :tabsize 4)
      (assoc :expandtab false)))

(defmethod init-file-type ".html" [buf] (init buf))
(defmethod init-file-type ".htm" [buf] (init buf))
(defmethod init-file-type ".xml" [buf] (init buf))
(defmethod init-file-type ".svg" [buf] (init buf))

