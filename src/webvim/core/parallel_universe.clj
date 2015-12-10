(ns webvim.core.parallel-universe)

(defn parallel-universe[] {})

(defn just-now[h]
  (peek (h :before)))

(defn next-future[h]
  (peek (h :after)))

(defn go-back
  ([h fnreverse]
  (let [event (just-now h)]
    (if (nil? event) h
      (-> h
          (update-in [:before] pop)
          (update-in [:after] conj (fnreverse event))))))
  ([h]
   (go-back h identity)))

(defn go-future
  ([h fnreverse]
   (let [event (next-future h)]
     (if (nil? event) h
       (-> h
           (update-in [:after] pop)
           (update-in [:before] conj (fnreverse event))))))
  ([h]
   (go-future h identity)))

(defn new-future[h event] ;in a new parallel universe
  (-> h
      (update-in [:before] conj event)
      (dissoc :after)))

(defn fast-forward[h]
  (-> h
      (assoc :before (into (h :before) (h :after)))
      (dissoc :after)))

(defn rewrite-history[h f]
  (-> h
      (update-in [:before] #(apply list (map f %)))
      (update-in [:after] #(apply list (map f %)))))

(defn no-future?[h]
  (-> h :after empty?))

;(-> (parallel-universe)
;    (new-future 1)
;    (new-future 1)
;    go-back
;    go-back
;    go-future)
