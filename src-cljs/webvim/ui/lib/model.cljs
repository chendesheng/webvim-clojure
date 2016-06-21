(ns webvim.ui.lib.model
  (:require [webvim.ui.lib.watcher :refer [watch-on watch-off trigger]]
            [clojure.string :as string]))

;only allow have one global model
(defn- keys-in [m]
  (if (or (and (not (map? m))
               (not (vector? m)))
          (empty? m))
    '(())
    (let [m (if (vector? m)
              (map-indexed (fn [i v] [i v]) m)
              m)]
      (for [[k v] m
            subkey (keys-in v)]
        (cons k subkey)))))

(defn apply-patch [data diff watchers]
  (let [new-data (merge data diff)]
    (if-not (empty? watchers)
      (doseq [path (keys-in diff)]
        (println path)
        (let [s (string/join "/" path)]
          (println s)
          (doseq [[re _] watchers]
            (let [captured (re-seq re s)]
              (println captured)
              (if-not (nil? (re-seq re s))
                (trigger watchers re
                         {:old-data data
                          :new-data new-data
                          :params (first captured)})))))))))

(defn- re-path [path]
  (re-pattern
    (str "^"
         (-> path
             (string/replace \. \/)
             (string/replace \* "(.*?)")))))

(defn on-model-change [watchers path watcher-id f]
  (watch-on watchers (re-path path) watcher-id
            (fn [{old-data :old-data
                  new-data :new-data
                  params :params}]
              (f params new-data old-data))))

(defn off-model-change [watchers path watcher-id]
  (watch-off watchers (re-path path) watcher-id))

