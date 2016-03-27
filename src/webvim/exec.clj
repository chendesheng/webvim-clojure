(ns webvim.exec
  (:require [me.raynes.fs :as fs]
            [clj-commons-exec :as exec]
            [webvim.core.editor :refer [current-working-directory]])
  (:import [org.apache.commons.exec
            ExecuteResultHandler
            LogOutputStream]))

(defrecord MyResultHandler [output]
  ExecuteResultHandler
  (onProcessComplete [_ _]
    (output "[Finish]\n"))
  (onProcessFailed [_ e]
    (output (format "[%s]\n" e))))

(defn exec-async [args output]
  (exec/sh args {:dir (current-working-directory)
                 :result-handler-fn (fn [result in out err opts]
                                      (->MyResultHandler output))
                 :out (proxy [LogOutputStream] nil
                        (processLine [line]
                          (output line)))})
  nil) ;hide result promise
