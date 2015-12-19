(ns webvim.core.register
  (:require [snipsnap.core :as clipboard])
  (:use webvim.core.event))

;global registers. Never access this directly, always use buffer's :registers instead
(defonce registers (atom {}))

(defn registers-get [regs ch]
  (if (= ch "+")
    (let [s (clipboard/get-text)]
      (swap! regs update-in ["+"] merge {:str s :linewise? (-> s last (= \newline))})))
  (@regs ch))

(defn registers-put! [regs ch v]
  (if (= ch "+")
    (clipboard/set-text! (v :str)))
  (swap! regs assoc ch v))

(defonce ^:private listen-new-buffer
  (listen :new-buffer
          (fn[buf]
            (assoc buf :registers registers))))
