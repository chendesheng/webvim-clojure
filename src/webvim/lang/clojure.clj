(ns webvim.lang.clojure
  (:require [cljfmt.core :as cljfmt]
            [me.raynes.fs :as fs]
            [webvim.keymap.objects :refer [current-word]]
            [webvim.panel :refer [append-output-panel]])
  (:use webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.line
        webvim.core.lang
        webvim.core.diff
        webvim.core.buffer
        webvim.core.ui
        webvim.keymap.ex
        webvim.indent
        webvim.keymap.compile
        webvim.core.utils))

(println "load clojure language")

(defn- init-clojure-file-type [buf]
  (-> buf
      (assoc-in [:language :id] ::clojure)
      (assoc-in [:language :name] "Clojure")
      (assoc :tabsize 2)
      (assoc :expandtab true)))

(defmethod init-file-type ".clj"
  [buf]
  (init-clojure-file-type buf))

(defmethod init-file-type ".edn"
  [buf]
  (init-clojure-file-type buf))

(defmethod word-re ::clojure [lang]
  (let [word-chars "a-zA-Z_\\-!.?+*=<>&#\\':0-9/"
        space-chars "\\s,"]
    {:word-chars word-chars
     :not-word-chars (str "^" word-chars)
     :space-chars space-chars
     :not-space-chars (str "^" space-chars)
     :punctuation-chars (str "^" word-chars space-chars)
     :not-punctuation-chars (str word-chars space-chars)}))

(defn- clojure? [buf]
  (-> buf :language :id (= ::clojure)))

(defn- indent-tab-size [^String s] 
  (or (contains?
        #{"try" "catch" "ns" "if" "nil?" "fn" "cond" "loop" "doseq" "for" "condp" "do" "binding"} s)
      (.startsWith s "def")
      (.startsWith s "if-")
      (.startsWith s "let")))

(defn clojure-comment? [line]
  (re-test #"^\s*;" line))

(defn clojure-not-blank-or-comment? [line]
  (not (or (rblank? line) (clojure-comment? line))))

(defn clojure-get-indent [line]
  (cond 
    (= 1 (count (.trim line)))
    1
    (not (= (char-at line 0) \())
    1
    (re-test #"^\(\s*[^,\s]+[\s,]+[^,\s]+" line)
    (let [w (re-subs #"[^\s\[\{\(]+"
                     (subr line 1 (count line)))]
      (if (indent-tab-size (str w))
        2
        (-> w count (+ 2))))
    :else 2))

(defn- comment-indent [r pos]
  (let [lines (ranges-to-texts r (pos-lines-seq+ r pos))
        line (or (some (fn [line]
                         (if (clojure-not-blank-or-comment? line) line))
                       lines) (first lines))]
    ;(println line)
    (or (re-subs #"^\s*" line) "")))

;find outer scope and align by start bracket
(defn clojure-indent
  "Indent by bracket parsing"
  [r pos]
  (let [[a b] (pos-line r pos)]
    (cond 
      (zero? a)
      ""
      (clojure-comment? (subr r a b))
      (comment-indent r pos)
      :else (let [tmp (reduce 
                        (fn [stack [a _]]
                          (let [ch (char-at r a)]
                            (if (and (contains? left-brackets ch) (empty? stack))
                              (reduced a)
                              (if (= (peek stack) (all-brackets ch))
                                (pop stack)
                                (conj stack ch))))) nil 
                        (pos-re-seq- r (dec a) #"(?<!\\)(\(|\[|\{|\}|\]|\))"))
                  mpos (if (number? tmp) tmp nil)]
              (if (nil? mpos)
                ""
                (let [ch (char-at r mpos)
                      [a b] (pos-line r mpos)
                      cnt (- mpos a)]
                  (repeat-chars 
                    (+ (- mpos a) 
                       (clojure-get-indent (subr r mpos b))) \space)))))))

(defmethod indent-pos ::clojure
  [lang r pos]
  (clojure-indent r pos))

(defn- cljfmt-diff [s name]
  (println "cljfmt-diff")
  (let [news (cljfmt/reformat-string s {:indents (assoc cljfmt/default-indents #".*" [[:block 0]])})
        tmpfile (str (fs/temp-file "" name))]
    (spit tmpfile s)
    ;TODO: (fs/delete tmpfile)
    (clojure.java.shell/sh 
      "diff" tmpfile "-" "-u"
      :in news)))

(defn- format-error [buf message]
  (async buf
         (update buf
                 :message
                 #(str %2 %3 " " %1)
                 "Format failed: " message)))

(defn- format-buffer [buf]
  ;use temp file
  (if (buf :dirty)
    (try
      (let [res (time (cljfmt-diff (-> buf :str str) (buf :name)))]
      ;FIXME: GNU diff exit code: 0: no diff, 1: has diff, 2: trouble
        (if (-> res :err empty?) 
          (-> buf
              (apply-line-changes
                (time (parse-diff (str (res :out)))))
              save-undo)
          (format-error (-> res :err str))))  ;use old buf if formatter fails
      (catch Exception exp
        (format-error buf (.getMessage exp))))
    buf))

(defn- print-eval [buf code]
  (append-output-panel 
    buf
    (format ":eval %s\n %s" code
            (with-out-str
              (->> code read-string eval str)))
    true))

(listen :normal-mode-keymap
        (fn [keymap buf]
          (if (clojure? buf)
            (wrap-key keymap
                      "K" (fn [handler]
                            (fn [buf keycode]
                              (let [word (current-word buf)]
                                (if (empty? word)
                                  (assoc buf :message "No string under cursor")
                                  (print-eval buf
                                              (str "(clojure.repl/doc " word ")"))))
                              buf)))
            keymap)))

(listen :init-ex-commands
        (fn [cmds buf]
          (if (clojure? buf)
            (wrap-command
              cmds
              "write" (fn [fnwrite]
                        (fn [buf cmd args]
                          (-> buf
                              (assoc :message "formatting...")
                              (async-with-catch
                                (-> buf
                                    format-buffer
                                    (fnwrite cmd args)
                                    buf-match-bracket))))))
            cmds)))
