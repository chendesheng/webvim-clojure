(ns webvim.core.register
  (:require [webvim.core.editor :refer [*window*]])
  (:use webvim.core.event
        webvim.core.utils))

(defonce ^:private registers (atom {}))

(listen
  :create-window
  (fn [window]
    (println "register create-window")
    (assoc window
           :registers (atom {}))))

(def ^:private unnamed-reg "\"")
(def ^:private black-hole-reg "_")
(def ^:private clipboard-reg "+")
(def ^:private clipboard-reg2 "*")
(def ^:private last-yank-reg "0")
(def ^:private small-delete-reg "-")

(defn- clipboard-upate-reg [ch]
  (let [s (clipboard-get)]
    (swap! registers update ch assoc :str s :linewise? (-> s last (= \newline)))))

(defn- window-registers []
  (*window* :registers))

(defn- put-window-registers [ch v]
  (swap! (window-registers) assoc ch v))

(defn registers-get [ch]
  (if (or (= clipboard-reg ch) (= clipboard-reg2 ch))
    (clipboard-upate-reg ch))
  (if (contains? #{"%" "#"} ch)
    (@(window-registers) ch)
    (@registers ch)))

(defn registers-put! [ch v]
  (cond
    (contains? #{clipboard-reg clipboard-reg2} ch)
    (clipboard-set! (v :str))
    (contains? #{"%" "#"} ch)
    (put-window-registers ch v)
    :else
    (let [append? (not (nil? (re-seq #"[A-Z]" ch)))
          ch (.toLowerCase ch)]
      (swap! registers assoc ch (if append?
                                  (let [{s :str linewise? :linewise?} v
                                        {s2 :str linewise2? :linewise?} (registers-get ch)]
                                    {:str (str s2 (if (and linewise? (not linewise2?)) "\n" "") s)
                                     :linewise? (or linewise? linewise2?)})
                                  v)))))

(defn map-registers [f]
  (map f (sort (merge @(window-registers) @registers))))

(defn registers-yank-to! [ch v]
  (registers-put! ch v)
  (if (and (not= ch black-hole-reg)
           (= ch unnamed-reg))
    (registers-put! last-yank-reg v)
    (registers-put! unnamed-reg v)))

(defn registers-delete-to! [ch v]
  (registers-put! unnamed-reg v)
  (if (not= ch unnamed-reg)
    (registers-put! ch v))
  (if (not= ch black-hole-reg)
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
      (if (->> v :str (re-seq #"\r?\n") count zero?)
        (registers-put! small-delete-reg v)))))
