(ns webvim.ui.lib.patch)

(defn- next-k [path k]
  (conj path (-> path first (get k))))

(defn- expand-children [children patch]
  (let [child-fn (fn [child]
                   (cond
                     (map? child) (constantly child)
                     (fn? child) child))]
    (mapcat (fn [[k v]]
              (let [a (child-fn (children :*))
                    b (child-fn (children k))]
                (cond
                  (nil? a) [[b k v]]
                  (nil? b) [[a k v]]
                  ;FIXME: Is this really needed?
                  :else [[a k v] [b k v]]))) patch)))

;; TODO: handling vector change, use a number as key. For example: {0 "update or delete FIRST element", -0.9 "insert before FIRST element", 1.1 "insert after SECOND element"} and watcher: {:+ inserted, :! updated, 0 first-element-updated}
(defn trigger-patch [watcher patch new-path old-path]
  (if-not (or (nil? watcher) (nil? patch))
    (let [children (watcher patch new-path old-path)]
      (if (and (map? patch) (map? children))
        (doseq [[w k v] (expand-children children patch)]
          (trigger-patch w v 
                         (next-k new-path k)
                         (next-k old-path k)))))))
