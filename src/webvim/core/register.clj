(ns webvim.core.register
  (:require [webvim.core.utils :refer [clipboard-get clipboard-set!]]))

(def ^:private unnamed-reg "\"")
(def ^:private black-hole-reg "_")
(def ^:private clipboard-reg "+")
(def ^:private clipboard-reg2 "*")
(def ^:private last-yank-reg "0")
(def ^:private small-delete-reg "-")

(defn- clipboard-upate-reg [registers ch]
  (let [s (clipboard-get)]
    (update registers ch assoc :str s :linewise? (-> s last (= \newline)))))

(defn- clipboard-reg? [ch]
  (or (= clipboard-reg ch) (= clipboard-reg2 ch)))

(defn registers-get [registers ch]
  (if (clipboard-reg? ch)
    (let [s (clipboard-get)]
      {:str s :linewise? (-> s last (= \newline))})
    (registers ch)))

(defn registers-put [registers ch v]
  (cond
    (clipboard-reg? ch)
    (do
      (clipboard-set! (v :str))
      registers)
    (contains? #{"%" "#"} ch)
    (assoc registers ch v)
    :else
    (let [append? (some? (re-seq #"[A-Z]" ch))
          ch (.toLowerCase ch)]
      (assoc registers ch (if append?
                            (let [{s :str linewise? :linewise?} v
                                  {s2 :str linewise2? :linewise?} (registers ch)]
                              {:str (str s2 (if (and linewise? (not linewise2?)) "\n" "") s)
                               :linewise? (or linewise? linewise2?)})
                            v)))))

(defn registers-yank-to [registers ch v]
  (if (and (not= ch black-hole-reg)
           (= ch unnamed-reg))
    (-> registers
        (registers-put ch v)
        (registers-put last-yank-reg v))
    (-> registers
        (registers-put ch v)
        (registers-put unnamed-reg v))))

(defn registers-delete-to [registers ch v]
  (let [regs1 (registers-put registers unnamed-reg v)
        regs2 (if (not= ch unnamed-reg)
                (registers-put regs1 ch v)
                regs1)]
    (if (not= ch black-hole-reg)
      (do
        ;Register 1: Last deletion. Register 2: Second last deletion. And so on.
        (let [regs3 (-> regs2
                        (assoc "1" v)
                        (assoc "2" (regs2 "1"))
                        (assoc "3" (regs2 "2"))
                        (assoc "4" (regs2 "3"))
                        (assoc "5" (regs2 "4"))
                        (assoc "6" (regs2 "5"))
                        (assoc "7" (regs2 "6"))
                        (assoc "8" (regs2 "7"))
                        (assoc "9" (regs2 "8")))]
          (if (->> v :str (re-seq #"\r?\n") count zero?)
            (registers-put regs3 small-delete-reg v)
            regs2))))))

(defn file-register [{name :name id :id filepath :filepath}]
  {:id id :str (or filepath name)})

