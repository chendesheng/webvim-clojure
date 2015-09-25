(ns webvim.register
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            [snipsnap.core :as clipboard])
  (:use webvim.core.event))

;global registers. Never access this directly, always use buffer's :registers instead
(defonce registers (atom {}))

(defn registers-get [regs ch]
  (if (= ch "+")
    (clipboard/get-text)
    (@regs ch)))

(defn registers-put [regs ch text]
  (if (= ch "+")
    (clipboard/set-text! text)
    (swap! regs assoc ch text)))

(listen :new-buffer
        (fn[buf]
          ;Local registers, atom. Set init value to global registers so it can share cross buffers.
          ;Use different registers in macro replaying to avoid side effect.
          (-> buf
              (assoc :macro {:recording-keys nil
                             ;which register will macro save to
                             :register ""})
              (assoc :registers registers))))
