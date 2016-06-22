(ns webvim.ui.lib.watcher)

(defn watch-on [watchers event key f]
  (assoc-in watchers [event key] f))

(defn watch-off [watchers event key]
  (update watchers event dissoc key))

(defn watch-once [watchers event f]
  (let [key (.getTime (new js/Date))]
    (watch-on watchers event key
              (fn [data]
                (try
                  (f data)
                  (finally (watch-off watchers event key)))))))

(defn watch-error [watchers key f]
  (watch-on watchers :error key
            (fn [data]
              (try
                (f data)
                (catch js/Error e
                  (println "error in side :error event:")
                  (println e))))))

(defn trigger [watchers event data]
  (doseq [[_ f] (watchers event)]
    ;TODO: try catch
    ;(try
    (f data)
    (comment catch js/Error e
             (let [d {:data data :error e}]
               (println e)
               (trigger watchers [event :error] d) 
               (trigger watchers :error d)))))
  ;)


