(ns webvim.lang.go
  (:require [cheshire.core :as json]
            [me.raynes.fs :as fs]
            [webvim.autoformat :refer [wrap-async-auto-format]]
            [webvim.panel :refer [append-output-panel]])
  (:use webvim.core.lang
        webvim.core.event
        webvim.core.rope
        webvim.core.diff
        webvim.core.line
        webvim.core.utils
        webvim.core.pos
        webvim.fuzzy
        webvim.exec
        webvim.autocompl
        webvim.keymap.compile
        webvim.keymap.ex
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
  [buf]
  (clang-indent buf))

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
  ;(println path)
  ;(println pos)
  (try
    (let [res (clojure.java.shell/sh "gocode" "-f=json"
                                     "autocomplete" path (str pos)
                                     :in stdin)
          [offset suggestions] (-> res :out (json/parse-string true))]
      suggestions)
    (catch Exception e
      (println e))))

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
                              ;(println "golang uncomplete-word")
                              ;(println w)
                              ;(println "[" (char-at r pos) "]")
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
                         (cons {:name ""} words)
                         (fuzzy-suggest w words)))
         :fn-words (fn [buf w]
                     (gocode-autocompl (-> buf :str str)
                                       (buf :filepath)
                                       (buf :pos)))
         :limit-number 0))

(listen :new-autocompl-provider
        (fn [provider buf]
          (if (golang? buf)
            (golang-autocompl provider)
            provider)))

(defn- directory [path]
  (if (fs/directory? path)
    path
    (subs path 0 (.lastIndexOf path java.io.File/separator))))

(defn project-path [filepath]
  (println GOPATH)
  (println filepath)
  (let [parent (.toLowerCase (str (fs/file GOPATH "src") java.io.File/separator))
        filepath (.toLowerCase filepath)]
    (if (.startsWith filepath parent)
      (subs (directory filepath) (count parent))
      nil)))

(defn cmd-go [buf _ _ args]
  (let [args (vec (clojure.string/split args #"\s+"))
        args1 (if (= (count args) 1)
                (if (-> args first (= "run"))
                  (conj args (buf :filepath))
                  (conj args (project-path (buf :filepath))))
                args)
        res (apply clojure.java.shell/sh (concat ["go"] args1 [:dir (str fs/*cwd*)]))]
    (-> buf
        (append-output-panel (res :out) false)
        (append-output-panel (res :err) true))))

(listen :init-ex-commands
        (fn [cmds buf]
          (if (golang? buf)
            (-> cmds
                (conj ["go" cmd-go])
                (wrap-async-auto-format format-buffer))
            cmds)))
