(ns webvim.autoformat
  (:require [webvim.keymap.ex :refer [wrap-command]]
            [clojure.java.shell :refer [sh]]
            [me.raynes.fs :as fs]
            [webvim.core.event :refer [fire-event]]
            [webvim.core.rope :refer [save-undo]]
            [webvim.core.utils :refer [windows? trim-last-newline with-temp-file]]
            [webvim.core.diff :refer [diff patch]]
            [webvim.core.editor :refer [async-update-buffer]]
            [webvim.core.buffer :refer [buf-match-bracket]]))

(defn- js-beautify [s name filetype]
  (println "js-beautify")
  (let [cmd-name (if windows? "js-beautify.cmd" "js-beautify")]
    (clojure.java.shell/sh cmd-name "--type" filetype :in s)))

(defn js-beautify-formatter [type]
  (fn [buf]
    ;use temp file
    ;FIXME: trim-last-newline is only a temp solution
    (let [s (-> buf :str str trim-last-newline)
          res (time (js-beautify s (buf :name) type))]
      (if (-> res :exit zero?)
        (-> buf
            (patch (diff s (-> res :out str)))
            save-undo)
        (do
          (println "Format Error:" (res :err))
          (-> res :err Throwable. throw))))));use old buf if formatter fails

(defn- format-error [buf message]
  (update buf
          :message
          #(str %2 %3 " " %1)
          "Format failed: " message))

(defn wrap-async-auto-format [cmds formatter]
  (letfn [(f [buf]
            (try
              (println "wrap-async-auto-format")
              (formatter buf)
              (catch Exception e
                (fire-event e :exception)
                (format-error buf (str e)))))]
    (wrap-command
      cmds
      "write" (fn [fnwrite]
                (fn [buf cmd rg args]
                  (-> buf
                      (assoc :message "formatting...")
                      (async-update-buffer
                        (fn [buf]
                          (-> buf
                              f
                              (fnwrite cmd rg args)
                              buf-match-bracket)))))))))

