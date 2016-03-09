(ns webvim.persistent
  (:require [webvim.core.event :refer [listen]]
            [webvim.core.buffer :refer [buffer-list new-file]]
            [me.raynes.fs :as fs]))

;use agent to avoid race condition
(defonce ^:private save-buffer-agent (agent {} :error-handler (fn [_ e] (println e))))

(defonce ^:private buffers-edn "buffers.edn")
(defonce ^:private config-dir (fs/file (fs/home) ".webvim"))

(defn- save-buffers! [buffers]
  (println "save-buffers!")
  (send save-buffer-agent
        (fs/mkdir config-dir)
        (spit (fs/file config-dir buffers-edn)
              (prn-str (map (fn [buf]
                              (select-keys buf [:filepath :y]))
                            (filter #(-> % :filepath nil? not)
                                    (map deref (vals buffers))))))))

(defn- read-buffers []
  (try
    (let [file (fs/file config-dir buffers-edn)]
      (if (fs/exists? file)
        (read-string (slurp file))))
    (catch Exception e
      (print e))))

(defn recover-buffers[]
  (doseq [{filepath :filepath
           y :y} (read-buffers)]
    (if (empty? (filter (fn[buf]
                          (= (buf :filepath) filepath))
                        (map deref (vals @buffer-list))))
      (new-file filepath y))))

(listen :write-buffer (fn [buf]
                        (save-buffers! @buffer-list)
                        buf))

(defn start-track []
  (add-watch buffer-list :save-buffers
             (fn [key _ _ buffers]
               (save-buffers! buffers)))) 


