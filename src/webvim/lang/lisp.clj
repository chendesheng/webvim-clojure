(ns webvim.lang.lisp
  (:require [cljfmt.core :as cljfmt]
            [clojure.string :as string]
            [me.raynes.fs :as fs]
            [webvim.core.lineindex :refer [range-by-line total-lines]]
            [webvim.keymap.objects :refer [current-word]]
            [webvim.panel :refer [append-output-panel]]
            [webvim.autoformat :refer [wrap-async-auto-format]])
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

(println "load lisp language")

(defn- init-lisp-file-type [buf]
  (-> buf
      (assoc-in [:language :id] ::lisp)
      (assoc-in [:language :name] "Lisp")
      (assoc :tabsize 2)
      (assoc :expandtab true)))

(defmethod init-file-type ".lisp"
  [buf]
  (init-lisp-file-type buf))

(defmethod init-file-type ".scm"
  [buf]
  (init-lisp-file-type buf))

(defmethod init-file-type ".el"
  [buf]
  (init-lisp-file-type buf))

(defmethod word-re ::lisp [lang]
  (let [word-chars "a-zA-Z_\\-!.?+*=<>&#%\\':0-9/"
        space-chars "\\s,"]
    {:word-chars word-chars
     :not-word-chars (str "^" word-chars)
     :space-chars space-chars
     :not-space-chars (str "^" space-chars)
     :punctuation-chars (str "^" word-chars space-chars)
     :not-punctuation-chars (str word-chars space-chars)}))

(defn- lisp? [buf]
  (-> buf :language :id (= ::lisp)))

(defn- indent-tab-size [^String s]
  (or (contains?
        #{"try" "catch" "ns" "if" "nil?" "fn" "cond" "loop" "doseq" "for" "condp" "do" "doto" "binding" "when" "case" "lambda"} s)
      (.startsWith s "with-")
      (.startsWith s "def")
      (.startsWith s "if-")
      (.startsWith s "when-")
      (.startsWith s "let")))

(defn- lisp-comment? [line]
  (re-test #"^\s*;" line))

(defn- lisp-not-blank-or-comment? [line]
  (not (or (rblank? line) (lisp-comment? line))))

(defn- lisp-get-indent [line]
  (cond
    (= 1 (count (.trim line)))
    1
    (not (= (char-at line 0) \())
    1
    (re-test #"^\(\s*(\(|\[|\{)" line)
    2
    (re-test #"^\(\s*[^,\s]+[\s,]+[^,\s]+" line)
    (let [w (re-subs #"[^\s\[\{\(]+"
                     (subr line 1 (count line)))]
      (if (indent-tab-size (str w))
        2
        (-> w count (+ 2))))
    :else 2))

(defn- comment-indent [{r :str :as buf}]
  (let [lines (ranges-to-texts r (pos-lines-seq+ buf))
        line (or (some (fn [line]
                         (if (lisp-not-blank-or-comment? line) line))
                       lines) (first lines))]
    ;(println line)
    (or (re-subs #"^\s*" line) "")))

(defn- drop-while-count [pred coll cnt]
  (let [s (seq coll)]
    (if (and s (pred (first s)))
      (recur pred (rest s) (dec cnt))
      [s cnt])))

(defn- seq-brackets [s pos]
  (let [[[bracket & s] pos] (drop-while-count (complement #{\( \) \[ \] \{ \}}) s pos)]
    (if (some? bracket)
      (loop [[ch & rs :as s] s
             cnt 0]
        (if (= ch \\)
          (recur rs (inc cnt))
          (concat
            (if (even? cnt)
              [[pos bracket]])
            (lazy-seq (seq-brackets s (- pos cnt 1)))))))))

;find outer scope and align by start bracket
(defn- lisp-indent
  "Indent by bracket parsing"
  [{r :str :as buf}]
  (let [[a b] (pos-line buf)]
    (cond
      (zero? a)
      ""
      (lisp-comment? (subr r a b))
      (comment-indent buf)
      :else (let [tmp (reduce
                        (fn [stack [a _]]
                          (let [ch (char-at r a)]
                            (if (and (contains? left-brackets ch) (empty? stack))
                              (reduced a)
                              (if (= (peek stack) (all-brackets ch))
                                (let [stack (pop stack)]
                                  (if (and (empty? stack) (= (char-at r (dec a)) \newline))
                                    (reduced nil)
                                    stack))
                                (conj stack ch))))) nil
                        (seq-brackets (rope-rseq r (dec a)) (dec a)))
                  mpos (if (number? tmp) tmp nil)]
              (if (nil? mpos)
                ""
                (let [[a b] (pos-line buf mpos)]
                  (repeat-chars
                    (+ (- mpos a)
                       (lisp-get-indent (subr r mpos b))) \space)))))))

(defmethod indent-pos ::lisp
  [buf]
  (lisp-indent buf))

