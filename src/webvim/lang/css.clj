(ns webvim.lang.css
  (:require [me.raynes.fs :as fs])
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

(defn- fix-last-newline [s]
  (if (re-test #"\R$" s) s (str s "\n")))

(defn- js-beautify [s name]
  (println "js-beautify")
  (let [res (clojure.java.shell/sh "js-beautify" "--type" "css" :in s)]
    (if (-> res :exit zero? not)
      res
      (let [tmpfile (str (fs/temp-file "" name))]
        (spit tmpfile (fix-last-newline s))
        ;TODO: (fs/delete tmpfile)
        (clojure.java.shell/sh 
          "diff" tmpfile "-" "-u"
          :in (fix-last-newline (res :out)))))))

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
