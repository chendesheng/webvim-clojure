(ns webvim.keymap.macro
  (:require [clojure.core.async :as async])
  (:use webvim.core.serve))

(defn- buf-close-chan-in![buf]
  (async/close! (:chan-in buf))
  buf)

(defn replay-keys [buf keycodes keymap]
  (let [buf1 (-> buf
               (assoc :chan-in (async/chan)) ;use local :chan-in :chan-out only for replay keys
               (assoc :chan-out (async/chan)))]
    ;(println "start replay:")
    ;(pprint keycodes)
    (key-server buf1 keymap)
    (let [buf2 (reduce 
                 (fn[buf1 kc]
                   (let[_ (async/>!! (buf1 :chan-in) kc) 
                        buf2 (async/<!! (buf1 :chan-out))]
                     (assoc buf2 
                            :changes
                            (concat (buf1 :changes) (buf2 :changes))))) buf1 keycodes)]
      (-> buf2
          buf-close-chan-in!
          ;restore back
          (assoc :chan-in (buf :chan-in))
          (assoc :chan-out (buf :chan-out))))))

