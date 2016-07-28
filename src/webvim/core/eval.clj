(ns webvim.core.eval
  (:require [webvim.core.utils :refer [with-temp-ns]]))

(defn- code-with-ns [ns code]
  (format
    "(webvim.core.utils/with-ns
      '%s
      (try
        %s
        (catch Exception e
          (println e))))"
    ns code))

(defn- code-sandbox [code]
  (format
    "(webvim.core.utils/with-temp-ns
       (require '[me.raynes.fs :as fs]
                '[clojure.pprint :refer :all]
                '[clojure.string :as string]
                '[clojure.set :as set]
                '[clojure.repl :refer :all])
                (try
                  %s
                  (catch Exception e
                    (println e))))"
    code))

(defn- eval-output [code]
  (try 
    (let [result (atom nil)
          output (with-out-str 
                   (->> code read-string eval str
                        (reset! result)))]
      {:output output
       :result @result})
    (catch Exception e
      {:exception e})))

(defn eval-with-ns [ns code]
  (eval-output (code-with-ns ns (str code))))

(defn eval-sandbox [code]
  (eval-output (code-sandbox (str code))))

