(ns webvim.lang.go
  (:require [cheshire.core :as json])
  (:use webvim.core.lang
        webvim.core.event
        webvim.core.rope
        webvim.core.diff
        webvim.core.line
        webvim.core.utils
        webvim.fuzzy
        clojure.pprint
        webvim.autocompl
        webvim.indent))

(println "load go language")

(defmethod init-file-type ".go"
  [buf]
  (-> buf
      (assoc-in [:language :id] ::go)
      (assoc-in [:language :name] "Go")
      (assoc :tabsize 4)
      (assoc :expandtab false)))

(defmethod indent-pos ::go
  [lang r pos]
  (clang-indent r pos))

(defmethod indent-trigger? ::go
  [lang keycode]
  (= keycode "}"))

(defn- golang? [buf]
  (-> buf :language :id (= ::go)))

(defn- format-buffer [buf]
  (let [res (clojure.java.shell/sh "goimports" "-d" :in (str (buf :str)))]
    (if (-> res :exit zero? not)
      (assoc buf :message (res :err))
      (-> buf
          (apply-line-changes (parse-diff (str (res :out))))
          save-undo))))

(def GOPATH (System/getenv "GOPATH"))
(def GOROOT (System/getenv "GOROOT"))

(defn- gocode-autocompl [stdin path pos]
  (println path)
  (println pos)
  (let [res (clojure.java.shell/sh "gocode" "-f=json"
                                   "autocomplete" path (str pos)
                                   :in stdin)
        [offset suggestions] (-> res :out (json/parse-string true))]
    (pprint (map :name suggestions))
    (map :name suggestions)))

(defn- golang-autocompl [provider]
  (println "golang-autocompl")
  (assoc provider
         :start-autocompl? (fn [buf keycode]
                             (println "golang-autocompl:" keycode)
                             (= keycode "."))
         :uncomplete-word (fn [buf] (or (buffer-uncomplete-word buf) ""))
         :fn-suggest (fn [w words]
                       (if (empty? w) (cons "" words)
                           (fuzzy-suggest w words)))
         :fn-words (fn [buf w]
                     (let [buf (buf-insert buf ".")]
                       (gocode-autocompl (-> buf :str str)
                                         (buf :filepath)
                                         (-> buf :pos))))))

(listen :new-autocompl-provider (fn [provider buf]
                                  (if (golang? buf)
                                    (golang-autocompl provider)
                                    provider)))

(listen :write-buffer (fn [buf]
                        (if (golang? buf)
                          (format-buffer buf)
                          buf)))
