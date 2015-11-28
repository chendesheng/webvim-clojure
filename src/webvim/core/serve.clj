(ns webvim.core.serve
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async])
  (:use clojure.pprint
        webvim.core.buffer))

(defn- record-keys[buf keycode]
  (if (nil? (#{"c+n" "c+p" "c+a" "c+x"} keycode)) ;Don't record keycode for these commands
    (update-in buf [:macro :recording-keys] conj keycode)
    buf))

(declare serve-keymap)
(declare map-fn-else)

(defn send-out
  "write buffer to out channel"
  [buf]
  (let [before (buf :before-send-out)
        after (buf :after-send-out)
        newbuf (before buf)]
    (async/>!! (newbuf :chan-out) newbuf)
    (after newbuf)))

(defn call-if-fn [buf f & args]
  (if (fn? f)
    (apply f buf args)
    buf))

;two types: key (leaf), keymap (internal node)
;when visit a keymap call :enter :leave 
; keymap is repetitive if :continue return true
;when visit a key call :before :after
(defn serve-keys
  "Serve a sequence of keys until end of keymap. Recusivly walk through keymap tree (works like sytax parser)"
  [buf keymap keycode]
  (let [f (keymap keycode)]
    (if (or (fn? f) (map? f) (fn? (:else keymap)))
      (-> buf
          (record-keys keycode)
          (call-if-fn (:before keymap) keycode)
          (map-fn-else keymap keycode)
          (call-if-fn (:after keymap) keycode))
      buf)))

(defn map-fn-else[buf keymap keycode]
  (let [f (keymap keycode)]
    (cond
      (map? f)
      (serve-keymap buf f keycode)
      (fn? f)
      (f buf)
      (nil? f)
      (call-if-fn buf (:else keymap) keycode))))

(defn loop-serve-keys[buf keymap]
  (let [keycode (async/<!! (:chan-in buf))
        buf1 (serve-keys buf keymap keycode)]
    (if (and (fn? (:continue keymap))
             ((:continue keymap) buf1 keycode))
      (recur (send-out buf1) keymap)
      [buf1 keycode])))

(defn serve-keymap[buf keymap keycode]
  (let [buf1 (-> buf
               (assoc :keys (conj (:keys buf) keycode))
               (call-if-fn (:enter keymap) keycode)
               send-out)
        [buf2 leave-keycode] (loop-serve-keys buf1 keymap)]
    (call-if-fn buf2 (:leave keymap) leave-keycode)))

(defn key-server-inner[buf keymap]
  (let [{in :chan-in out :chan-out} buf]
    (try
      (let [keycode (async/<!! in)]
        (if (nil? keycode)
          (async/close! out)
          (-> buf
              (serve-keys keymap keycode)
              (assoc :keys [])
              send-out)))
      (catch Exception e
        (let [err (str "caught exception: " e)]
          (println err)
          (.printStackTrace e)
          (send-out (merge buf {:mode 0 :message err}) ))))))

(defn key-server
  "Start a goroutine handle input keys. Close :chan-in will stop it"
  ([buf keymap]
   (async/go 
     (loop[buf1 buf]
       (if (not (nil? buf1))
         (recur (key-server-inner 
                  buf1
                  ;use buf's :root-keymap instead of buf1's
                  ;change :root-keymap must restart key-server
                  keymap)))))
   buf)
  ([buf]
   (key-server buf (buf :root-keymap))))

;enter point of key sequence parser
(defonce root-keymap (atom {}))

(defn- set-root-keymap[buf]
  [buf]
  (if (-> buf :root-keymap nil?)
    (assoc buf :root-keymap @root-keymap)
    buf))

(defn new-file[f]
  (-> f
      open-file
      set-root-keymap
      buffer-list-save!
      ;start a new goroutine handle this file
      key-server))
