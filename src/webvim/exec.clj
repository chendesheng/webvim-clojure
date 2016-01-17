(ns webvim.exec
  (:require [me.raynes.fs :as fs]
            [clj-commons-exec :as exec])
  (:import [org.apache.commons.exec
            ExecuteResultHandler
            LogOutputStream]))

(defrecord MyResultHandler [output]
  ExecuteResultHandler
  (onProcessComplete [_ _]
    (output "[Finish]"))
  (onProcessFailed [_ e]
    (output (format "[%s]" e))))

(defn exec-async[args output]
  (exec/sh args {:dir (str fs/*cwd*)
                 :result-handler-fn (fn[result in out err opts]
                                      (->MyResultHandler output))
                 :out (proxy [LogOutputStream] nil
                             (processLine[line]
                               (output line)))})
  nil) ;hide result promise
