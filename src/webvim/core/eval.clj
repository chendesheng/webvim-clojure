(ns webvim.core.eval
  (:require [webvim.core.utils :refer [with-temp-ns]]))

(defn- temp-namespace [ns code]
  (format
    "(webvim.core.utils/with-temp-ns
       (require '[me.raynes.fs :as fs]
                '[clojure.pprint :refer :all]
                '[clojure.string :as string]
                '[clojure.set :as set]
                '[clojure.repl :refer :all]
                %s)
                %s)"
    (if (nil? ns)
      ""
      (format "'[%s :refer :all]" ns))
    code))

(defn eval-refer-ns [^String ns ^String code]
  (try 
    (let [result (atom nil)
          output (with-out-str 
                   (->> code (temp-namespace ns) read-string eval str
                        (reset! result)))]
      {:output output
       :result @result})
    (catch Exception e
      {:exception e})))
