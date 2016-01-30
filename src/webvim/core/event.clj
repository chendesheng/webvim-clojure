(ns webvim.core.event)

;str change event
;require listener has at least 1 arity and return buffer
(defonce listeners (atom {}))

(defn listen[typ handler]
  (swap! listeners update-in [typ] conj handler))

(defn fire-event
  ([typ b arg]
    (reduce (fn[b f]
              (f b arg)) b (@listeners typ)))
  ([b typ]
    (reduce (fn[b f]
              (f b)) b (@listeners typ)))
  ([b oldb c typ] ;new old change
    (reduce (fn[b f]
              (f b oldb c)) b (@listeners typ))))

;(fire-str-changes {} :str-change)
