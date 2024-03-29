(ns webvim.core.parallel-universe)

(defn parallel-universe [] {})

(defn just-now [h]
  (peek (:before h)))

(defn next-future [h]
  (peek (:after h)))

(defn go-back
  ([h fnreverse]
    (let [event (just-now h)]
      (if (nil? event) h
          (-> h
              (update :before pop)
              (update :after conj (fnreverse event))))))
  ([h]
    (go-back h identity)))

(defn go-future
  ([h fnreverse]
    (let [event (next-future h)]
      (if (nil? event) h
          (-> h
              (update :after pop)
              (update :before conj (fnreverse event))))))
  ([h]
    (go-future h identity)))

(defn new-future [h event] ;in a new parallel universe
  (-> h
      (update :before conj event)
      (dissoc :after)))

(defn fast-forward [h]
  (-> h
      (assoc :before (into (h :before) (h :after)))
      (dissoc :after)))

(defn rewrite-history [h f]
  (-> h
      (update :before #(apply list (map f %)))
      (update :after #(apply list (map f %)))))

(defn no-future? [h]
  (-> h :after empty?))

;(-> (parallel-universe)
;    (new-future 1)
;    (new-future 1)
;    go-back
;    go-back
;    go-future)
