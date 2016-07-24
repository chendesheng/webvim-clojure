(ns webvim.lang.clojure
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

(defmethod init-file-type ".cljs"
  [buf]
  (init-clojure-file-type buf))

(defmethod init-file-type ".cljc"
  [buf]
  (init-clojure-file-type buf))

(defmethod init-file-type ".edn"
  [buf]
  (init-clojure-file-type buf))

(defmethod word-re ::clojure [lang]
  (let [word-chars "a-zA-Z_\\-!.?+*=<>&#%\\':0-9/"
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
        #{"try" "catch" "ns" "if" "nil?" "fn" "cond" "loop" "doseq" "for" "condp" "do" "doto" "binding" "when"} s)
      (.startsWith s "def")
      (.startsWith s "if-")
      (.startsWith s "when-")
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
                         (if (clojure-not-blank-or-comment? line) line))
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
(defn clojure-indent
  "Indent by bracket parsing"
  [{r :str :as buf}]
  (let [[a b] (pos-line buf)]
    (cond 
      (zero? a)
      ""
      (clojure-comment? (subr r a b))
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
                       (clojure-get-indent (subr r mpos b))) \space)))))))

(defmethod indent-pos ::clojure
  [buf]
  (clojure-indent buf))

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
    (let [res (time (cljfmt-diff (-> buf :str str) (buf :name)))]
      ;FIXME: GNU diff exit code: 0: no diff, 1: has diff, 2: trouble
      (if (-> res :err empty?) 
        (-> buf
            (apply-line-changes
              (time (parse-diff (str (res :out)))))
            save-undo)
        (-> res :err str Throwable. throw))) ;use old buf if formatter fails
    buf))

(listen :normal-mode-keymap
        (fn [keymap buf]
          (if (clojure? buf)
            (wrap-key keymap
                      "K" (fn [handler]
                            (fn [buf keycode]
                              (let [word (current-word buf)]
                                ;(println "empty:" (empty? word))
                                (if (empty? word)
                                  (assoc buf :message "No string under cursor")
                                  (print-eval buf
                                              (format "(doc %s)" word)))))))
            keymap)))

(listen :init-ex-commands
        (fn [cmds buf]
          (if (clojure? buf)
            (wrap-async-auto-format cmds format-buffer)
            cmds)))

(def ^:private keywords
  (str
    "def defonce cond apply if-not if-let if not not= = < > <= >= ==  / * - rem " 
    "quot neg? pos? delay? symbol? keyword? true? false? integer? empty? coll? list? " 
    "set? ifn? fn? associative? sequential? sorted? counted? reversible? number? decimal? " 
    "class? distinct? isa? float? rational? reduced? ratio? odd? even? char? seq? vector? " 
    "string? map? nil? some some? contains? zero? instance? not-every? not-any? libspec? -> ->> .. . " 
    "inc compare do dotimes mapcat take remove take-while drop letfn drop-last take-last " 
    "drop-while while intern condp case reduced cycle split-at split-with repeat replicate " 
    "iterate range merge zipmap declare line-seq sort comparator sort-by dorun doall nthnext " 
    "nthrest partition eval doseq await await-for let agent atom send send-off release-pending-sends " 
    "add-watch mapv map-indexed filterv remove-watch agent-error restart-agent set-error-handler error-handler " 
    "set-error-mode! error-mode shutdown-agents quote var fn loop recur throw try catch monitor-enter " 
    "monitor-exit defmacro defn defn- macroexpand macroexpand-1 for dosync and or " 
    "when when-not when-let comp juxt partial sequence memoize constantly complement identity assert " 
    "peek pop doto proxy defstruct first rest cons defprotocol cast coll deftype defrecord last butlast " 
    "sigs reify second ffirst fnext nfirst nnext defmulti defmethod meta with-meta ns in-ns create-ns import " 
    "refer keys select-keys vals key val rseq name namespace promise into transient persistent! conj! " 
    "assoc! dissoc! pop! disj! use class type num float double short byte boolean bigint biginteger " 
    "bigdec print-method print-dup throw-if printf println print format load compile get-in update update-in pr pr-on newline " 
    "flush read slurp spit read-line subvec with-open memfn time re-find re-groups rand-int rand mod locking " 
    "assert-valid-fdecl alias resolve ref deref refset swap! reset! set-validator! compare-and-set! alter-meta! " 
    "reset-meta! commute get-validator alter ref-set ref-history-count ref-min-history ref-max-history ensure sync io! " 
    "new next conj set! to-array future future-call into-array aset gen-class reduce reduce-kv map filter find empty " 
    "hash-map hash-set sorted-map sorted-map-by sorted-set sorted-set-by vec vector seq flatten reverse assoc assoc-in dissoc list " 
    "disj get aget union difference intersection extend extend-type extend-protocol int nth delay count concat chunk chunk-buffer " 
    "chunk-append chunk-first chunk-rest max min dec unchecked-inc-int unchecked-inc unchecked-dec-inc unchecked-dec unchecked-negate " 
    "unchecked-add-int unchecked-add unchecked-subtract-int unchecked-subtract chunk-next chunk-cons chunked-seq? prn vary-meta " 
    "lazy-seq spread list* str find-keyword keyword symbol gensym force rationalize finally" 
    ;not in clojure.core
    " defproject defroutes"))

(defn- re-keywords [keywords]
  (let [not-word-chars "^a-zA-Z_\\-!.?+*=<>&#$\\':0-9/"
        re-start (str "(?<=^|[" not-word-chars "])")
        re-end (str "(?=[" not-word-chars "]|$)")]
    (re-pattern
      (str 
        re-start
        "(\\Q"
        (string/join "\\E|\\Q" (string/split keywords #"\s+"))
        "\\E)"
        re-end))))

(defn tokenize [line]
  (let [m (re-matcher (re-keywords keywords) line)]
    (loop [i 0
           res []]
      (if (.find m)
        (let [a (.start m)
              b (.end m)
              res (if (< i a)
                    (conj res [:clj i a])
                    res)]
          (recur b (conj res [:clj.keyword a b])))
        (if (< i (count line))
          (conj res [:clj i (count line)])
          res)))))

(defn parse-lines [{r :str
                    lidx :lineindex} la lb]
  (println "parse-lines:" la lb)
  (reduce
    (fn [scopes i]
      (conj scopes
            (tokenize (subr r (range-by-line lidx i))))) [la] (range la lb)))
;(tokenize "(defn[hello])")

(listen :change-buffer
        (fn [buf oldbuf {[ax ay] :a [bx by] :b}]
          (async-with-catch
            buf (assoc buf
                       :scope-changes
                       (parse-lines
                         buf ay
                         (let [lines (-> buf :lineindex total-lines)
                               oldlines (-> oldbuf :lineindex total-lines)]
                           (println "scope-changes:" lines oldlines)
                           (if (> lines oldlines)
                             (+ ay (- lines oldlines) 1)
                             (inc ay))))))))

(listen :new-buffer
        (fn [buf]
          (assoc buf
                 :scope-changes
                 (parse-lines buf 0 (-> buf :lineindex total-lines)))))
