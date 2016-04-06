(ns webvim.core.event
  (:require [clj-time.format :as tf]
            [clj-time.core :refer [now]]))

;str change event
;require listener has at least 1 arity and return buffer
(defonce listeners (atom {}))

(defn listen [typ handler]
  (let [id (str *ns*)]
    (swap! listeners update typ assoc id handler)))

(defn fire-event
  ([typ b arg]
    (reduce-kv (fn [b k f]
                 (f b arg)) b (@listeners typ)))
  ([b typ]
    (reduce-kv (fn [b k f]
                 (f b)) b (@listeners typ)))
  ([b oldb c typ] ;new old change
    (reduce-kv (fn [b k f]
                 (f b oldb c)) b (@listeners typ))))

;(fire-str-changes {} :str-change)
(defmacro log [obj]
  `(fire-event :log
               (with-out-str
                 (printf "src/%s:%s [%s]"
                         ~*file* ~(:line (meta &form))
                         (tf/unparse (tf/formatter "hh:mm:ss") (now)))) ~obj))
