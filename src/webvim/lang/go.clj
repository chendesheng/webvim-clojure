(ns webvim.lang.go
  (:require [cheshire.core :as json])
  (:use webvim.core.lang
        webvim.core.event
        webvim.core.rope
        webvim.core.diff
        webvim.core.line
        webvim.core.utils
        webvim.core.pos
        webvim.fuzzy
        webvim.autocompl
        clojure.pprint
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
  (assoc provider
         :start-autocompl? (fn [buf keycode]
                             ;(println "golang-autocompl:" keycode)
                             ;(= keycode ".")
                             (let [ch (keycode-to-char keycode)]
                               ;(println "golang-autocompl")
                               ;(println keycode)
                               ;(println ch)
                               ;(println (re-test ch #"[\w.]"))
                               (if (nil? ch)
                                 false
                                 (re-test ch #"[\w.]"))))
         :continue-autocompl? (fn [buf keycode]
                                (let [ch (keycode-to-char keycode)]
                                  (if (nil? ch)
                                    true
                                    (re-test ch #"\w"))))
         :uncomplete-word (fn [buf]
                            (let [w (buffer-uncomplete-word buf)
                                  r (buf :str)
                                  pos (buf :pos)]
                              (println "golang uncomplete-word")
                              (println w)
                              (println "[" (char-at r pos) "]")
                              (if (nil? w)
                                (let [pos (dec pos)]
                                          ;return empty if last not-word-char is "."
                                          ;return nil ends current autocompl
                                  (if (and (>= pos 0) (= (char-at r pos) \.))
                                    ""
                                    nil))
                                w)))
         :fn-suggest (fn [w words]
                       (if (= w "")
                         (cons "" words)
                         (fuzzy-suggest w words)))
         :fn-words (fn [buf w]
                     (gocode-autocompl (-> buf :str str)
                                       (buf :filepath)
                                       (buf :pos)))
         :limit-number 0))

(listen :new-autocompl-provider (fn [provider buf]
                                  (if (golang? buf)
                                    (golang-autocompl provider)
                                    provider)))

(listen :write-buffer (fn [buf]
                        (if (golang? buf)
                          (format-buffer buf)
                          buf)))
