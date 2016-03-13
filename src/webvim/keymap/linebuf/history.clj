(ns webvim.keymap.linebuf.history
  (:use webvim.core.rope
        webvim.core.pos
        webvim.core.parallel-universe
        webvim.core.utils))

(defn- set-line-buffer [buf s]
  (-> buf
      (assoc-in [:line-buffer :str] (rope s))
      (assoc-in [:line-buffer :pos] (count s))))

(defn- recover-command-history [buf history]
  (let [s (next-future history)]
    (println "next-future:" s)
    (if (nil? s) 
      buf
      (set-line-buffer buf s))))

(defn- save-history! [ahistory s]
  (if-not (or (empty? s) (= (just-now @ahistory) s))
    (swap! ahistory 
           #(-> %
                fast-forward
                (new-future s)))))


(defn- save-current [history buf]
  (println "save-current:" (history :current))
  (if (-> history :current nil?)
    (assoc history :current (select-keys
                              (-> buf :line-buffer)
                              [:str :pos]))
    history))

(defn- restore-current [buf history]
  (println "restore-current:" (history :current))
  (if (-> history :current nil?)
    buf
    (update-in buf [:line-buffer] merge (history :current))))

(defn init-history-keymap [ahistory]
  (let [ahistory (or ahistory (atom (parallel-universe)))]
    {:leave (fn [buf keycode]
              (let [linebuf (buf :line-buffer)]
                ;(println "leave:" linebuf)
                ;remove :line-buffer or :line-buffer :str can prevent current operation save to history
                (if-not (nil? linebuf)
                  (save-history! ahistory
                                 (-> linebuf :str str))))
              buf)
     "<c-p>" (fn [buf keycode]
               (swap! ahistory (fn [h]
                                 (-> h
                                     (save-current buf)
                                     go-back)))
               (recover-command-history buf @ahistory))
     "<c-n>" (fn [buf keycode]
               (println "c-n")
               (swap! ahistory go-future)
               (if (no-future? @ahistory)
                 (restore-current buf @ahistory)
                 (recover-command-history buf @ahistory)))
     "<esc>" (fn [buf keycode] (dissoc buf :line-buffer))})) ;prevent save to history
