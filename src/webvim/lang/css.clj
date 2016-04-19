(ns webvim.lang.css
  (:require [me.raynes.fs :as fs]
            [webvim.core.utils :refer [windows?]])
  (:use webvim.core.lang
        webvim.core.diff
        webvim.core.event
        webvim.core.rope
        clojure.pprint
        webvim.indent))

(defmethod init-file-type ".css"
  [buf]
  (-> buf
      (assoc-in [:language :id] ::css)
      (assoc-in [:language :name] "CSS")
      (assoc :tabsize 4)
      (assoc :expandtab true)))

(defmethod indent-pos ::css
  [lang r pos]
  (clang-indent r pos))

(defmethod indent-trigger? ::css
  [lang keycode]
  (= keycode "}"))

(defn- js-beautify [s name]
  (println "js-beautify")
  (let [cmd-name (if windows? "js-beautify.cmd" "js-beautify")
        res (clojure.java.shell/sh cmd-name "--type" "css" :in s)]
    (if (-> res :exit zero? not)
      res
      (let [tmpfile (str (fs/temp-file "" name))]
        (spit tmpfile s)
        ;TODO: (fs/delete tmpfile)
        (clojure.java.shell/sh 
          "diff" tmpfile "-" "-u"
          :in (res :out))))))

(defn- format-buffer [buf]
  ;use temp file
  (if (-> buf :language :id (= ::css))
    (let [res (time (js-beautify (-> buf :str str) (buf :name)))]
      (if (-> res :err empty?) 
        (-> buf
            (apply-line-changes
              (time (parse-diff (str (res :out)))))
            save-undo)
        (do
          (println "Format Error:" (res :err))
          (assoc buf :message (res :err)))))  ;use old buf if formatter fails
    buf))

(listen :write-buffer
        (fn [buf]
          (format-buffer buf)))
