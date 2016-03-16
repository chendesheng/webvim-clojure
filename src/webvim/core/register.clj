(ns webvim.core.register
  (:use webvim.core.event
        webvim.core.utils))

(defonce ^:private registers (atom {}))

(def ^:private unnamed-reg "\"")
(def ^:private black-hole-reg "_")
(def ^:private clipboard-reg "+")
(def ^:private clipboard-reg2 "*")
(def ^:private last-yank-reg "0")
(def ^:private small-delete-reg "-")

(defn- clipboard-upate-reg [ch]
  (let [s (clipboard-get)]
    (swap! registers update ch assoc :str s :linewise? (-> s last (= \newline)))))

(defn registers-get [ch]
  (if (or (= clipboard-reg ch) (= clipboard-reg2 ch))
    (clipboard-upate-reg ch))
  (@registers ch))

(defn registers-put! [ch v]
  (if (or (= clipboard-reg ch) (= clipboard-reg2 ch))
    (clipboard-set! (v :str)))
  (swap! registers assoc ch v))

(defn map-registers [f]
  (map f (sort @registers)))

(defn registers-yank-to! [ch v]
  (registers-put! ch v)
  (if (and (not= ch black-hole-reg)
           (= ch unnamed-reg))
    (registers-put! last-yank-reg v)
    (registers-put! unnamed-reg v)))

;TODO: A-Z append
(defn registers-delete-to! [ch v]
  (registers-put! ch v)
  (if (and (not= ch black-hole-reg)
           (= ch unnamed-reg))
    (do
      ;Register 1: Last deletion. Register 2: Second last deletion. And so on.
      (swap! registers 
             (fn [registers]
               (-> registers
                   (assoc "1" v)
                   (assoc "2" (registers "1"))
                   (assoc "3" (registers "2"))
                   (assoc "4" (registers "3"))
                   (assoc "5" (registers "4"))
                   (assoc "6" (registers "5"))
                   (assoc "7" (registers "6"))
                   (assoc "8" (registers "7"))
                   (assoc "9" (registers "8")))))
      (if (->> v :str (re-seq #"\R") count zero?)
        (registers-put! small-delete-reg v)))
    (registers-put! unnamed-reg v)))
