(ns webvim.exec
  (:require [me.raynes.fs :as fs]
            [clj-commons-exec :as exec])
  (:import [org.apache.commons.exec LogOutputStream]))

(defn exec-async[args output]
  (exec/sh args {:dir (str fs/*cwd*)
                 :out (proxy [LogOutputStream] nil
                             (processLine[line]
                               (output line)))}))
