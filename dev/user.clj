(ns user
  (:require [me.raynes.fs :as fs]
            [cljfmt.core :as cljfmt]
            [webvim.panel :refer [append-output-panel]]
            [clojure.string :as string]
            [webvim.server :as server]
            [webvim.main :refer [start]]
            [webvim.keymap.ex :refer [print-eval]])
  (:use clojure.pprint
        clojure.repl
        webvim.core.buffer
        webvim.core.ui
        webvim.core.rope
        webvim.core.register
        webvim.core.event
        webvim.core.utils
        webvim.keymap))

(comment defn print-buf
         ([]
           (let [buf (dissoc (get-from-ui :buf) :str :history :keymap :normal-mode-keymap
                             :insert-mode-keymap :lineindex :ex-mode-keymap :context)]
             (pprint buf)))
         ([& ks]
           (pprint (select-keys (get-from-ui :buf) ks))))

(defn- cache-resource [path url]
  (if-not (fs/exists? path)
    (spit path (slurp url))))

;I don't like include js library directly, but also don't want download it over and over.
(defn- cache-resources []
  (doseq [r []]
    (apply cache-resource r)))

(defn restart []
  (future
    (Thread/sleep 10) ;wait some time so restart happens after flush states to client
    (server/stop)
    (start false {:port 8080 :join? false}))
  "ok")

(defn eval-reload [^String ns]
  (try
    (->> (format "(use '%s :reload)" ns) read-string eval)
    (catch Exception e
      (str e))))

(defn- reload-keymap [buf]
  (let [tmp (init-keymap-tree buf)
        keymaps (assoc tmp :keymap (tmp :normal-mode-keymap))]
    (merge buf keymaps)))

(defn- reload-keymaps [{bufid :id :as buf}]
  (-> buf
      reload-keymap
      (update-in [:window :buffers] #(map-vals reload-keymap %))))

(defn- cmd-reload [buf _ _ _]
  (let [ns (get-namespace (buf :filepath))
        ret (if (nil? ns)
              "Can't get right namespace"
              (eval-reload ns))]
    (-> buf
        reload-keymaps
        (assoc :message
               (if (nil? ret)
                 (restart)
                 (str ret))))))

(defn- cmd-doc [buf _ _ [name]]
  (let [code (str "(doc " name ")")]
    (print-eval buf code)))

(defn- cmd-print [buf _ _ props]
  (let [code (str "(user/print-buf " (string/join " " props) ")")]
    (print-eval buf code)))

(comment defn- cmd-test [buf _ _ props]
         (let [ns (get-namespace (buf :filepath))
               output (str (repeat-chars 80 \=) "\n:test\n"
                           (with-out-str
                             (binding [clojure.test/*test-out* *out*]
                               (clojure.test/run-tests (symbol ns)))))]
           (append-output-panel buf output true)))

(defn add-init-ex-commands-event []
  (listen :init-ex-commands
          (fn [cmds _]
            (conj cmds
                  ["reload" cmd-reload]
                  ;["print" cmd-print]
                  ["doc" cmd-doc]))))

(defn- get-files []
  (let [dir? #(.isDirectory %)]
    (tree-seq dir? #(.listFiles %) fs/*cwd*)))

(defn rename [from to]
  (fs/rename from to))

(defn delete [f]
  (fs/delete f))

(defn format-all-js []
  (doseq [f (filter (fn [f]
                      (clojure.string/ends-with? f ".css")) (get-files))]
    (println f)
    (clojure.java.shell/sh "js-beautify" "-r" "-f" (str f))))

(defn- format-clj-file [f]
  (let [s (slurp f)
        news (cljfmt/reformat-string s {:indents (assoc cljfmt/default-indents #".*" [[:block 0]])})]
    (spit f news)))

(defn format-all-clojure []
  (doseq [f (filter (fn [f]
                      (clojure.string/ends-with? f ".clj")) (get-files))]
    (println f)
    (format-clj-file (str f))))

(comment defn reset-buffers []
         (let [buffers (get-buffers persistent-buffers)]
           (reset-buffers!)
           (registers-put! "%" nil)
           (doseq [{filepath :filepath y :y} buffers]
             (let [buf @(new-file filepath y)]
               (cond
                 (and (-> (get-from-ui :buf) :filepath some?)
                      (path= filepath ((get-from-ui :buf) :filepath)))
                 (do
                   (update-ui (fn [ui] (dissoc ui :buf)))
                   (registers-put! "%" {:str filepath :id (buf :id)})
                   (send-buf! buf))
                 (path= filepath (:str (registers-get "#")))
                 (registers-put! "#" {:str filepath :id (buf :id)}))))
           (if (nil? (registers-get "%"))
             (let [buf (first (get-buffers))]
               (registers-put! "%" {:str (buf :filepath) :id (buf :id)})
               (send-buf! buf)))))

;Not sure why agent await blocking everything. Start a java thread works fine.
(defonce main
  (do
    (cache-resources)
    (add-init-ex-commands-event)
    (start
      true
      {:port 8080 :join? false})))

(defn model-handler [diff data]
  (println "ROOT")
  {:cwd (fn [diff data]
          (println "cwd"))
   :buffers (fn [diff data]
              (println "buffers")
              {:* (fn [key diff data]
                    (println "buffers.*")
                    (println key)
                    {:* (fn [key diff data]
                          (println "buffers.*.*")
                          (println key))
                     :changes {:0 "change"
                               :1 "change2"
                               :+0 "change3"
                               :-0 "change4"}
                     :pos (fn [diff data]
                            (println "buffers.*.pos"))
                     :scroll-top (fn [diff data]
                                   (println "buffers.*.scroll-top"))
                     :str (fn [diff data]
                            (println "buffers.*.str"))})})})

(defn trigger-patch
  ([data patch handler key]
    (if-not (nil? patch)
      (let [children (if (nil? key)
                       (handler patch data)
                       (handler key patch data))]
        (if (and (map? patch)
                 (map? children))
          (doseq [[k v] patch]
            (let [child* (children :*)]
              (if (fn? child*)
                (trigger-patch (get data k) v child* k)))
            (let [child (children k)]
              (if (fn? child)
                (trigger-patch (get data k) v child))))))))
  ([data patch handler]
    (trigger-patch data patch handler nil)))

(trigger-patch nil {:cwd "hello"
                    :buffers {:1 {:pos 1 :scroll-top 1}
                              :2 {:pos 2 :scroll-top 2}}} model-handler)
