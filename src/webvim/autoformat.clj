(ns webvim.autoformat
  (:require [webvim.keymap.ex :refer [wrap-command]]
            [clojure.java.shell :refer [sh]]
            [me.raynes.fs :as fs]
            [webvim.core.event :refer [fire-event]]
            [webvim.core.rope :refer [save-undo]]
            [webvim.core.utils :refer [windows? trim-last-newline]]
            [webvim.core.diff :refer [apply-line-changes parse-diff]]
            [webvim.core.buffer :refer [async-with-catch buf-match-bracket async]]))

(defn- js-beautify [s name filetype]
  (println "js-beautify")
  (let [cmd-name (if windows? "js-beautify.cmd" "js-beautify")
        res (clojure.java.shell/sh cmd-name "--type" filetype :in s)]
    (if (-> res :exit zero? not)
      res
      (let [tmpfile (str (fs/temp-file "" name))]
        (spit tmpfile s)
        ;TODO: (fs/delete tmpfile)
        (clojure.java.shell/sh 
          "diff" tmpfile "-" "-u"
          :in (res :out))))))

(defn js-beautify-formatter [type]
  (fn [buf]
    ;use temp file
    ;FIXME: trim-last-newline is only a temp solution
    (let [res (time (js-beautify (-> buf :str str trim-last-newline) (buf :name) type))]
        ;FIXME: GNU diff exit code: 0: no diff, 1: has diff, 2: trouble
      (if (-> res :err empty?) 
        (-> buf
            (apply-line-changes
              (time (parse-diff (str (res :out)))))
            save-undo)
        (do
          (println "Format Error:" (res :err))
          (-> res :err Throwable. throw))))));use old buf if formatter fails

(defn- format-error [buf message]
  (async buf
         (update buf
                 :message
                 #(str %2 %3 " " %1)
                 "Format failed: " message)))

(defn wrap-async-auto-format [cmds formatter]
  (letfn [(f [buf]
            (try
              (formatter buf)
              (catch Exception e
                (fire-event e :exception)
                (format-error buf (str e)))))]
    (wrap-command
      cmds
      "write" (fn [fnwrite]
                (fn [buf cmd args]
                  (-> buf
                      (assoc :message "formatting...")
                      (async-with-catch
                        (-> buf
                            f
                            (fnwrite cmd args)
                            buf-match-bracket))))))))
