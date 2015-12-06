(ns webvim.lang.javascript
  (:use webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.line
        webvim.core.lang
        webvim.indent
        webvim.core.utils))

(println "load javascript language")

(defonce ^:private listen-new-buffer
  (listen
    :load-language
    (fn [buf]
      (if (= (buf :ext) ".js")
        (-> buf
            (assoc-in [:language :id] ::javascript)
            (assoc-in [:language :name] "JavaScript"))
        buf))))

(def re-js-statements #"\b(if|while|switch|for)\s*\(.*?\)\s*$")

(defn clang-comment? [line]
  (re-test #"^\s*//" line))

(defn clang-not-blank-or-comment? [line]
  (not (or (rblank? line) (clang-comment? line))))

;1. indent -1: line contains brace but pline not
;   if {
;       aaaa    <- pline
;   }           <- line 
;
;2. indent +1: pline contains brace but line not
;   if {        <- pline
;       aaaa    <- line
;   }
;
;3. keep indent: both contains braces or both not
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
        pbrace? (re-test #"[\{]\s*$" pline)
        brace? (re-test #"^\s*\}" line)]
    (cond (clang-comment? line)
          (auto-indent r pos)
          (empty? pline)
          ""
          (and (not pbrace?) brace?)
          (if (empty? pindent) "" (subs pindent 1))
          (and pbrace? (not brace?))
          (str pindent "\t")
          (and pbrace? brace?)
          pindent
          (re-test re-js-statements pline)
          (str pindent "\t")
          :else
          (let [ppline (nth lines 3 "")
                ppindent (re-subs #"^\s*" ppline)]
            (if (and (re-test re-js-statements ppline)
                     (not (re-test #"[\}]\s*$" pline)))
              ppindent
              pindent)))))

(defmethod indent-pos ::javascript
  [lang r pos]
  (clang-indent r pos))

(defmethod indent-trigger? ::javascript
  [lang keycode]
  (= keycode "}"))
