(ns webvim.persistent
  (:require [webvim.core.event :refer [listen]]
            [webvim.core.ui :refer [ui-agent send-buf!]]
            [webvim.core.register :refer [registers-put!]]
            [webvim.core.buffer :refer [new-file get-buffers get-buffer-by-filepath buffer-list]]
            [me.raynes.fs :as fs]))

;use agent to avoid race condition
(defonce ^:private save-buffer-agent (agent {} :error-handler
                                            (fn [_ err]
                                              (println "save-buffer-agent failed:")
                                              (println err))))

(defonce ^:private buffers-edn "buffers.edn")
(defonce ^:private config-dir (fs/file (fs/home) ".webvim"))

(defn- save-buffers! [buffers active]
  (println "save-buffers!")
  (send save-buffer-agent
        (fn [_]
          (fs/mkdir config-dir)
          (spit (fs/file config-dir buffers-edn)
                (prn-str {:buffers (map #(select-keys % [:filepath :y])
                                        (filter #(-> % :filepath nil? not) buffers))
                          :active (-> active :filepath)})))))

(defn- read-buffers []
  (try
    (let [file (fs/file config-dir buffers-edn)]
      (if (fs/exists? file)
        (read-string (slurp file))))
    (catch Exception e
      (print e))))

(defn recover-buffers []
  (let [{buffers :buffers active :active} (read-buffers)]
    (doseq [{filepath :filepath y :y} buffers]
      (if (nil? (get-buffer-by-filepath filepath))
        (new-file filepath y)))
    ;recover active buffer
    (let [buf (get-buffer-by-filepath active)]
      (if (-> buf nil? not)
        (do
          (registers-put! "%" {:str active :id (buf :id)})
          (send-buf! buf))))))

(listen :write-buffer
        (fn [buf]
          (save-buffers! (get-buffers) (@ui-agent :buf))
          buf))

(defn start-track []
  (add-watch ui-agent :save-buffers
             (fn [_ _ oldui newui]
               (if (not= (-> oldui :buf :filepath) (-> newui :buf :filepath))
                 (save-buffers! (get-buffers) (newui :buf)))))
  (add-watch buffer-list :save-buffers
             (fn [_ _ _ buffers]
               (save-buffers! (map deref buffers) (@ui-agent :buf)))))

