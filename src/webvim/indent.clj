(ns webvim.indent
  (:use clojure.pprint
        (clojure [string :only (join split blank?)])
        webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.line
        webvim.core.lang
        webvim.core.utils))

(defn auto-indent 
  [r pos]
  (let [lines (ranges-to-texts r (pos-lines-seq- r pos))
        line (first lines)
        pline (->> lines rest (filter #(-> % rblank? not)) first)]
    (if (nil? pline) ""
        (or (re-subs #"^\s*" pline) ""))))

(defn- buf-indent-line [buf pos]
  (let [r (buf :str)
        ;_ (println (buf :language))
        indent (indent-pos (buf :language) r pos) 
        a (pos-line-first r pos)
        b (pos-line-start r pos)]
    ;(println a b (count indent))
    (buf-replace buf a b indent)))

(defn buf-indent-lines 
  "indent from cursor row to row both inclusive. Only indent not blank line."
  [buf [a b]]
  (let [r (buf :str)
        lines (filter #(not (rblank? r %)) 
                      (pos-lines-seq+ r a b))
        buf (buf-set-pos buf (-> lines first first))
        lang (buf :language)]
    (line-start
      (first (reduce
               (fn [[buf delta] [pos _]]
                 (let [r (buf :str)
                       a (+ pos delta)
                       b (pos-line-start r a)
                       indent (indent-pos lang r a)]
                   [(buf-replace buf a b indent)
                    ;shift after buffer changed pos
                    (+ delta (- (count indent) (- b a)))])) 
               [buf 0]
               lines)))))

(defn- line-str [r pos]
  (subr r (pos-line r pos)))

(defn buf-indent-current-line
  [buf]
  (let [{r :str pos :pos} buf
        before (line-str r pos)
        buf (buf-indent-line buf pos)
        after (line-str (buf :str) pos)]
    ;press 'o' 'O' then '<esc>' cancel auto indent of cursor line.
    (if (and (= (count before) 1) (> (count after) 1) (rblank? after))
      (update buf :last-indents conj (pos-line (buf :str) pos))
      buf)))

(def re-c-statements #"\b(if|while|switch|for|struct)\s*\(.*?\)\s*$")

(defn clang-comment? [line]
  (re-test #"^\s*//" line))

(defn clang-not-blank-or-comment? [line]
  (not (or (rblank? line) (clang-comment? line))))

;1. indent -1: line contains bracket but pline not
;   if {
;       aaaa    <- pline
;   }           <- line 
;
;2. indent +1: pline contains bracket but line not
;   if {        <- pline
;       aaaa    <- line
;   }
;
;3. keep indent: both contains brackets or both not
;   if {        <- pline
;   }           <- line
;   if {
;       aaaa    <- pline
;       bbbb    <- line
;   }
;
;4. indent +1
;   if (aa==bb) <- pline
;       aaaaa;  <- line
;
;5. indent -1
;   if (aa==bb) <- ppline
;       aaaaa;  <- pline
;   bbbbb;      <- line
;
;6. function {
;       hello(aa==bb)  <- ppline
;   }                  <- pline
;   aaaa               <- line
(defn clang-indent [r pos]
  (let [[head & ranges] (pos-lines-seq- r pos)
        line (subr r head)
        lines (filter clang-not-blank-or-comment?
                      (ranges-to-texts r ranges))
        _ (println (str "[" line "]"))
        pline (or (first lines) "")
        _ (println (str "[" pline "]"))
        pindent (str (re-subs #"^\s*" pline))
        pbracket? (re-test #"[\{]\s*$" pline)
        bracket? (re-test #"^\s*\}" line)]
    (cond (clang-comment? line)
          (auto-indent r pos)
          (empty? pline)
          ""
          (and (not pbracket?) bracket?)
          (if (empty? pindent) "" (subs pindent 1))
          (and pbracket? (not bracket?))
          (str pindent "\t")
          (and pbracket? bracket?)
          pindent
          (re-test re-c-statements pline)
          (str pindent "\t")
          :else
          (let [ppline (nth lines 3 "")
                ppindent (re-subs #"^\s*" ppline)]
            (if (and (re-test re-c-statements ppline)
                     (not (re-test #"[\}]\s*$" pline)))
              ppindent
              pindent)))))

(defmethod indent-pos :default
  [lang r pos]
  (auto-indent r pos))

(defmethod indent-trigger? :default
  [lang keycode]
  false)
