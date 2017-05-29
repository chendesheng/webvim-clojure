(ns webvim.core.views
  (:require [webvim.core.event :refer [fire-event listen]]
            [webvim.core.utils :refer [map-vals]]
            [clojure.zip :as zip]))

(listen :create-window
        (fn [window]
          (assoc window :views 0)))

(defonce gen-view-id (atom 0))
(defn view-id []
  (swap! gen-view-id inc))

(defn- find-view [views viewid]
  (loop [v (zip/vector-zip views)]
    (cond
      (zip/branch? v)
      (recur (zip/next v))
      (= (zip/node v) viewid)
      v
      (zip/end? v)
      nil
      :else
      (recur (zip/next v)))))

(defn- update-view [views viewid fn-update]
  (loop [v (zip/vector-zip views)]
    (cond
      (zip/branch? v)
      (recur (zip/next v))
      (= (zip/node v) viewid)
      (zip/root (fn-update v))
      (zip/end? v)
      views
      :else
      (recur (zip/next v)))))

(defn split-view [window view bufid direction size append?]
  (let [new-view (view-id)]
    (-> window
        (update :views
                update-view
                view
                (fn [views]
                  (zip/edit
                    views
                    (fn [view]
                      (zip/make-node
                        views view
                        (if append?
                          [direction size view new-view]
                          [direction size new-view view]))))))
        (assoc-in [:buffers bufid :view] new-view))))

(defn- get-sibling-view [views]
  (let [r (zip/right views)]
    (if (some? r)
      r
      (zip/left views))))

;remove root does nothing
(defn remove-view [window view]
  (let [viewloc (find-view (window :views) view)
        toexpand (get-sibling-view viewloc)
        new-activebuf (if (some? toexpand)
                        (some (fn [buf]
                                (if (-> buf :view (= (zip/node toexpand)))
                                  (buf :id)))
                              (-> window :buffers vals))
                        (window :active-buffer))]
    (-> window
        (update :buffers
                (fn [buffers]
                  (map-vals (fn [buf]
                              (if (-> buf :view (= view))
                                (dissoc buf :view)
                                buf)) buffers)))
        (update :views
                (fn [_]
                  (let [parent (zip/up viewloc)]
                    (if (some? parent)
                      (->> toexpand
                           (zip/replace parent)
                           zip/root)
                      (zip/root viewloc)))))
        (assoc :active-buffer new-activebuf))))

(defn- find-next-view [views viewloc]
  (loop [viewloc (zip/next viewloc)]
    (if (integer? (zip/node viewloc))
      viewloc
      (if (zip/end? viewloc)
        (recur (zip/vector-zip views))
        (recur (zip/next viewloc))))))

(defn next-view [window view]
  (println "next-view:" view)
  (let [views (window :views)
        viewloc (find-view views view)
        nextviewloc (find-next-view views viewloc)
        nextview (zip/node nextviewloc)]
    (println "nextviewloc" nextviewloc)
    (if (= nextview (-> window :buffers (get (window :active-buffer)) :view))
      window
      (assoc window
             :active-buffer
             (some
               (fn [buf]
                 (if (-> buf :view (= nextview))
                   (buf :id)))
               (-> window :buffers vals))))))


