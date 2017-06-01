(ns webvim.core.syntax
  (:require [webvim.core.event :refer [listen]]
            [webvim.core.rope :refer [ranges-to-texts subr]]
            [webvim.core.line :refer [pos-lines-seq+]]
            [webvim.core.utils :refer [vconj splice]]
            [webvim.core.lineindex :refer [range-by-line pos-linenum total-lines]]
            [clojure.string :as string])
  (:import (me.textmate.main Registry)))

(listen :create-window
        (fn [window]
          (println "create registry")
          (assoc window :registry (Registry.))))

(defn load-grammar [registry path]
  (println "load-grammar:" registry)
  (.loadGrammarFromPathSync registry path))

(defn- token2vec [token]
  [(.-startIndex token)
   (.-endIndex token)
   (seq (.-scopes token))])

(defn- tokenize-line
  ([grammar line continuation]
    (let [result (.tokenizeLine grammar line continuation)]
      [(.-ruleStack result)
       (->> result .-tokens seq (map token2vec))]))
  ([grammar line]
    (tokenize-line grammar line nil)))

(defn tokenize-all [buf]
  (let [lines (map str
                   (ranges-to-texts
                     (buf :str)
                     (pos-lines-seq+ buf 0)))
        [scopes rule-stacks] (loop [[line & lines] lines
                                    continuation nil
                                    scopes []
                                    rule-stacks []]
                               (if (some? line)
                                 (let [[rule-stack tokens] (tokenize-line (buf :grammar) (str line) continuation)]
                                   (recur lines
                                          rule-stack
                                          (conj scopes tokens)
                                          (conj rule-stacks rule-stack)))
                                 [scopes rule-stacks]))]
    (assoc buf
           :scopes scopes
           :scope-changes [[0 scopes]]
           :rule-stacks rule-stacks)))

(defn- set-rule-stack [buf n rule-stack]
  (assoc-in buf [:rule-stacks n] rule-stack))

(defn- set-scopes [buf n tokens]
  (assoc-in buf [:scopes n] tokens))

(defn re-tokenize [{rule-stacks :rule-stacks
                    grammar :grammar
                    lidx :lineindex
                    r :str
                    [linea lineb] :changeLines
                    :as buf}]
  (println "re-tokenize" linea lineb)
  (if (nil? linea)
    buf
    (loop [buf buf
           n linea
           continuation (get rule-stacks linea)]
      (if (or (>= n (total-lines lidx))
              (and (>= n lineb)
                   (.equals continuation (get rule-stacks n))))
        (update buf :scope-changes vconj [linea (-> buf :scopes (subvec linea n))])
        (let [line (str (subr r (range-by-line lidx n)))
              [rule-stack tokens] (tokenize-line grammar line continuation)]
          (recur (-> buf
                     (set-rule-stack n continuation)
                     (set-scopes n tokens))
                 (inc n)
                 rule-stack))))))

(listen :buffer-changed
        (fn [buf _ _]
          (-> buf
              re-tokenize
              (dissoc :changeLines))))

(listen :change-buffer
        (fn [{lidx :lineindex :as buf} {old-lidx :lineindex :as old-buf} {pos :pos len :len to :to :as c}]
          (if (-> buf :rule-stacks nil?)
            buf
            (let [linea (pos-linenum lidx pos)
                  lineb (->> to count (+ pos) inc (pos-linenum lidx))
                  old-linea linea
                  old-lineb (->> pos (+ len) inc (pos-linenum old-lidx))]
              (println "update rule-stacks" c linea lineb old-linea old-lineb)
              (-> buf
                  (update :changeLines (fn [[a b :as rg]]
                                         (if (nil? rg)
                                           [linea lineb]
                                           (let [a (cond
                                                     (< a linea) a
                                                     (>= a lineb) (+ a (- (- lineb linea) (- old-lineb old-linea)))
                                                     :else linea)
                                                 b (cond
                                                     (< b linea) b
                                                     (>= b lineb) (+ b (- (- lineb linea) (- old-lineb old-linea)))
                                                     :else linea)]
                                             [(min a linea) (max b lineb)]))))
                  (update :rule-stacks
                          (fn [rule-stacks]
                            (splice rule-stacks
                                    old-linea old-lineb
                                    (repeat (- lineb linea) nil))))
                  (update :scopes
                          (fn [scopes]
                            (splice scopes
                                    old-linea old-lineb
                                    (repeat (- lineb linea) nil)))))))))

