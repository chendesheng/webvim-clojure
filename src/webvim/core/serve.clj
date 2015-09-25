(ns webvim.core.serve
  (:require [me.raynes.fs :as fs]
            [ring.adapter.jetty :as jetty]
            [clojure.core.async :as async])
  (:use clojure.pprint
        webvim.global
        webvim.core.buffer))

(defn- record-keys[b keycode]
  (if (nil? (#{"c+n" "c+p" "c+a" "c+x"} keycode)) ;Don't record keycode for these commands
    (update-in b [:macro :recording-keys] conj keycode)
    b))

(declare serve-keymap)
(declare map-fn-else)

(defn send-out
  "write buffer to out channel"
  [obj out]
  (let[b (buf-bound-scroll-top obj)]
    (async/>!! out b)
    (assoc b :changes [])))

;two types: key (leaf), keymap (internal node)
;when visit a keymap call :enter :leave 
; keymap is repetitive if :continue return true
;when visit a key call :before :after
(defn serve-keys
  "Serve a sequence of keys until end of keymap. Recusivly walk through keymap tree (works like sytax parser)"
  [b keymap keycode]
  (let [f (keymap keycode)]
    (if (or (fn? f) (map? f) (fn? (:else keymap)))
      (-> b
          (record-keys keycode)
          (call-if-fn (:before keymap) keycode)
          (map-fn-else keymap keycode)
          (call-if-fn (:after keymap) keycode))
      b)))

(defn map-fn-else[b keymap keycode]
  (let [f (keymap keycode)]
    (cond
      (map? f)
      (serve-keymap b f keycode)
      (fn? f)
      (f b)
      (nil? f)
      (call-if-fn b (:else keymap) keycode))))

(defn loop-serve-keys[b keymap]
  (let [keycode (async/<!! (:chan-in b))
        b1 (serve-keys b keymap keycode)]
    (if (and (fn? (:continue keymap))
             ((:continue keymap) b1 keycode))
      (recur (send-out b1 (:chan-out b1)) keymap)
      [b1 keycode])))

(defn serve-keymap[b keymap keycode]
  (let [b1 (-> b
               (buffer-append-keys keycode)
               (call-if-fn (:enter keymap) keycode)
               (send-out (:chan-out b)))
        [b2 leave-keycode] (loop-serve-keys b1 keymap)]
    (call-if-fn b2 (:leave keymap) leave-keycode)))

(defn key-server-inner[b keymap]
  (let [{in :chan-in out :chan-out} b]
    (try
      (let [keycode (async/<!! in)]
        (if (nil? keycode)
          (async/close! out)
          (-> b
              (serve-keys keymap keycode)
              buffer-reset-keys
              (send-out out))))
      (catch Exception e
        (let [err (str "caught exception: " e)]
          (println err)
          (.printStackTrace e)
          (send-out (merge b {:ex "" :mode 0 :message err}) 
                    out))))))

(defn key-server
  "Start a dedicate thread handle input keys. Close :chan-in will stop this thread."
  [b keymap]
  (async/thread 
    (loop[b1 b]
      (if (not (nil? b1))
        (recur (key-server-inner 
                 b1
                 keymap)))))
  b)

;enter point of key sequence parser
(defonce root-keymap (atom {}))

(defn new-file[f]
  (-> f
      open-file
      buffer-list-save
      ;start a new thread handle this file
      (key-server @root-keymap)))
