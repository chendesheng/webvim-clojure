;This namespace contains most global shared objects like active-buffer-id, buffer-list etc. as long as some common util functions
(ns webvim.global
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [clojure.java.io :as io])
  (:use clojure.pprint
        (clojure [string :only (join split)])))

;one server only serve one window at one time
(defonce window (atom{:viewport {:w 0 :h 0}}))

;generate buffer id and buffer id only
(defonce gen-buf-id (atom 0))

;key: buffer id, value: buffer map
(defonce buffer-list (atom {}))

;the current editting buffer, when receving keys from client send keycode to this buffer's :chan-in
;switch to another buffer is very easy, just do (reset! actvive-buffer b)
;TODO Serve more than one client at once, make eash session different active-buffer
(defonce active-buffer-id (atom int))

(defn active-buffer[]
  (@buffer-list @active-buffer-id))

(defn pprint2
  "call pprint and return first argument, works with in \"->\" macro"
  ;TODO find out a way to print out caller's function name and file name so that we can get rid of the prefix argument.
  ;TODO get a better name
  [obj prefix]
  (let[]
    (println prefix)
    (pprint obj)
    obj))