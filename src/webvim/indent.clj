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
  [{r :str :as buf}]
  (let [lines (ranges-to-texts r (pos-lines-seq- buf))
        line (first lines)
        pline (->> lines rest (filter #(-> % rblank? not)) first)]
    (if (nil? pline) ""
        (or (re-subs #"^\s*" pline) ""))))

(defn- buf-indent-line [buf]
  (let [indent (indent-pos buf)
        a (pos-line-first buf)
        b (pos-line-start buf)]
    ;(println a b (count indent))
    (buf-replace buf a b indent)))

(defn- indent-size [indent {expandtab? :expandtab
                            tabsize :tabsize}]
  (if expandtab?
    (visual-size indent tabsize)
    (count indent)))

(defn buf-indent-lines 
  "indent from cursor row to row both inclusive. Only indent not blank line."
  [buf [a b]]
  ;(log "buf-indent-lines:")
  ;(log [a b])
  (let [r (buf :str)
        lidx (buf :lineindex)
        lines (filter #(not (rblank? r %)) 
                      (pos-lines-seq+ buf a b))
        lang (buf :language)]
    ;(log (vec lines))
    (first (reduce
             (fn [[buf delta] [pos _]]
               (let [r (buf :str)
                     a (+ pos delta)
                     b (pos-line-start buf a)
                     indent (indent-pos (assoc buf :pos a))]
                 ;(log [(str r) indent a b (char-at r a) (pos-line-first r pos) (indent-size indent buf) delta])
                 [(buf-replace buf a b indent)
                  ;shift after buffer changed pos
                  (+ delta (- (indent-size indent buf) (- b a)))])) 
             [buf 0]
             lines))))

(defn buf-indent-current-line
  [buf]
  (let [{pos :pos} buf
        line-first (pos-line-first buf)
        before (line-str buf)
        buf (buf-indent-line buf)
        ;line-first will remain in the same row after indent
        after (line-str buf line-first)]
    ;press 'o' 'O' then '<esc>' cancel auto indent of cursor line.
    (if (and (= (count before) 1) (> (count after) 1) (rblank? after))
      (update buf :last-indents conj (pos-line buf))
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
(defn clang-indent [{r :str :as buf}]
  (let [[head & ranges] (pos-lines-seq- buf)
        line (subr r head)
        lines (filter clang-not-blank-or-comment?
                      (ranges-to-texts r ranges))
        ;_ (println (str "[" line "]"))
        pline (or (first lines) "")
        ;_ (println (str "[" pline "]"))
        pindent (str (re-subs #"^\s*" pline))
        pbracket? (re-test #"[\{]\s*$" pline)
        bracket? (re-test #"^\s*\}" line)]
    (cond (clang-comment? line)
          (auto-indent buf)
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
  [buf]
  (auto-indent buf))

(defmethod indent-trigger? :default
  [lang keycode]
  false)
