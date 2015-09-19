(ns webvim.indent
  (:use clojure.pprint
        (clojure [string :only (join split blank?)])
        webvim.text
        webvim.change
        webvim.line
        webvim.global))

(def re-js-statements #"\b(if|while|switch|for)\s*\(.*?\)\s*$")

;trim leading spaces but not '\n'
;only use in indent function
(defn trim-left-space[line]
  (.replaceAll line "^[ \t]+" ""))

(def indent-tab-size #{"def" "defn" "if" "fn" "let" "cond" "loop"})

(defn auto-indent 
  [s pos]
  (let [lines (filter #(-> % text-blank? not)
                      (ranges-to-texts s (lines-reverse s pos)))
        line (first lines)
        pline (second lines)]
    (if (nil? pline) ""
      (or (re-subs #"^\s*" pline) ""))))

(defn clojure-comment? [line]
  (re-test #"^\s*;" line))

(defn clojure-not-blank-or-comment? [line]
  (not (or (text-blank? line) (clojure-comment? line))))

(defn clojure-get-indent[line]
  (cond 
    (= 1 (count (.trim line)))
    1
    (not (= (text-char-at line 0) \())
    1
    (re-test #"^\(\s*[^,\s]+[\s,]+[^,\s]+" line)
    (let [w (re-subs #"[^\s\[\{\(]+"
                     (text-subs line 1))]
      (if (contains? indent-tab-size (str w))
        2
        (-> w count (+ 2))))
    :else 2))

;find outer scope and align by start bracket
(defn clojure-indent
  "Indent by brace parsing"
  [s pos]
  (let [[a b] (pos-line s pos)]
    (cond 
      (zero? a)
      ""
      (clojure-comment? (text-subs s a b))
      (auto-indent s pos)
      :else (let [tmp (reduce 
                        (fn[stack [a _]]
                          (let [ch (text-char-at s a)]
                            (if (and (contains? left-braces ch) (empty? stack))
                              (reduced a)
                              (if (= (peek stack) (all-braces ch))
                                (pop stack)
                                (conj stack ch))))) nil (pos-re-backward-seq (dec a) s re-braces))
                  mpos (if (number? tmp) tmp nil)]
              (if (nil? mpos)
                ""
                (let [ch (text-char-at s mpos)
                      [a b] (pos-line s mpos)
                      cnt (- mpos a)]
                  (repeat-space 
                    (+ (- mpos a) 
                       (clojure-get-indent (text-subs s mpos b))))))))))

(defn clang-comment? [line]
  (re-test #"^\s*//" line))

(defn clang-not-blank-or-comment? [line]
  (not (or (text-blank? line) (clang-comment? line))))

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
(defn clang-indent [s pos]
  (let [[head & ranges] (lines-reverse s pos)
        line (text-subs-range s head)
        lines (filter clang-not-blank-or-comment?
                      (ranges-to-texts s ranges))
        _ (println (str "[" line "]"))
        pline (or (first lines) "")
        _ (println (str "[" pline "]"))
        pindent (re-subs #"^\s*" pline)
        pbrace? (re-test #"[\{]\s*$" pline)
        brace? (re-test #"^\s*\}" line)]
    (cond (clang-comment? line)
          (auto-indent s pos)
          (empty? pline)
          ""
          (and (not pbrace?) brace?)
          (if (empty? pindent) "" (text-subs pindent 1))
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

(defn buf-indent-line[t pos]
  (let [s (t :str)
        indent ((-> t :language :fn-indent) s pos)
        a (pos-line-first pos s)
        b (pos-line-start pos s)]
    (println a b)
    (text-replace t a b indent)))

(defn buf-indent-lines 
  "indent from cursor row to row both inclusive"
  [t newpos]
  (let [s (t :str)
        [p1 p2] (sort2 (t :pos) newpos)]
    (loop [t (text-update-pos t p2) ;put pos at end and keep track it
           [a _] (pos-first-line s p1 #(not (range-blank? s %)))] ;start at first non-blank line
      (println (t :pos) a)
      (if (< (t :pos) a) (text-update-pos t p1)
        (let [t1 (buf-indent-line t a)
              s1 (t1 :str)]
          (recur 
            t1
            ;use a as start, a doesn't change when indent current line
            (pos-next-line s1 a #(not (range-blank? s1 %)))))))))

(defn buf-indent-current-line
  [b]
  (buf-indent-line b (b :pos)))
