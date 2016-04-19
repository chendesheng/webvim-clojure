(ns webvim.lang.javascript
  (:require [me.raynes.fs :as fs]
            [webvim.core.utils :refer [windows?]])
  (:use webvim.core.lang
        webvim.core.rope
        webvim.core.diff
        webvim.core.event
        clojure.pprint
        webvim.indent))

(println "load javascript language")

(defmethod init-file-type ".js"
  [buf]
  (-> buf
      (assoc-in [:language :id] ::javascript)
      (assoc-in [:language :name] "JavaScript")
      (assoc :tabsize 4)
      (assoc :expandtab true)))

(defmethod indent-pos ::javascript
  [lang r pos]
  (clang-indent r pos))

(defmethod indent-trigger? ::javascript
  [lang keycode]
  (= keycode "}"))

(defn- js-beautify [s name]
  (println "js-beautify")
  (let [cmd-name (if windows? "js-beautify.cmd" "js-beautify")
        res (clojure.java.shell/sh cmd-name :in s)]
    (if (-> res :exit zero? not)
      res
      (let [tmpfile (str (fs/temp-file "" name))]
        (spit tmpfile s)
        ;TODO: (fs/delete tmpfile)
        (clojure.java.shell/sh 
          "diff" tmpfile "-" "-u"
          :in (res :out))))))

(defn- jsfmt [s]
  (println "jsfmt")
  (clojure.java.shell/sh "jsfmt" "-d" :in s))

;(defn- pprint2[buf]
;  (println "buffer:")
;  (pprint (buf :str))
;  buf)

(defn- format-buffer [buf]
  ;use temp file
  (if (-> buf :language :id (= ::javascript))
    (let [res (time (js-beautify (-> buf :str str) (buf :name)))]
    ;(let [res (time (jsfmt (-> buf :str str)))]
      ;FIXME: GNU diff exit code: 0: no diff, 1: has diff, 2: trouble
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
