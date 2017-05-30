(ns webvim.core.syntax
  (:require [webvim.core.event :refer [listen]]
            [webvim.core.rope :refer [ranges-to-texts]]
            [webvim.core.line :refer [pos-lines-seq+]]
            [clojure.string :as string])
  (:import (me.textmate.main Registry)))

(defn load-grammar [path]
  (let [registry (Registry.)]
    (.loadGrammarFromPathSync registry path)))

(defn- token2vec [token]
  [(.-startIndex token)
   (.-endIndex token)
   (seq (.-scopes token))])

(defn tokenize-all [buf]
  (println "tokenize-all:")
  (println (buf :str))
  (let [lines (map str
                   (ranges-to-texts
                     (buf :str)
                     (pos-lines-seq+ buf 0)))
        [scopes rule-stacks] (loop [[line & lines] lines
                                    continuation nil
                                    scopes []
                                    rule-stacks []]
                               (if (some? line)
                                 (let [result (.tokenizeLine (buf :grammar) (str line) continuation)]
                                   (recur lines
                                          (.-ruleStack result)
                                          (->> result .-tokens seq (map token2vec) (conj scopes))
                                          (conj rule-stacks (.-ruleStack result))))
                                 [scopes rule-stacks]))]
    (assoc buf
           :scopes scopes
           :scope-changes [0 scopes]
           :rule-stacks rule-stacks)))

(defn tokenize-changes [buf changes]
  buf)

