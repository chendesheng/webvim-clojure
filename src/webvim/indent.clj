(ns webvim.indent
  (:use clojure.pprint
        (clojure [string :only (join split blank?)])
        webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.line
        webvim.utils))

(def re-braces #"(?<!\\)(\(|\[|\{|\}|\]|\))")
(defn repeat-space[n]
  (reduce (fn[s _]
            (str s " ")) 
          ""
          (range 0 n)))

(def re-js-statements #"\b(if|while|switch|for)\s*\(.*?\)\s*$")

;trim leading spaces but not '\n'
;only use in indent function
(defn trim-left-space[line]
  (.replaceAll line "^[ \t]+" ""))

(def indent-tab-size #{"def" "defn" "if" "fn" "let" "cond" "loop"})

(defn auto-indent 
  [r pos]
  (let [lines (filter #(-> % rblank? not)
                      (ranges-to-texts r (pos-lines-seq- r pos)))
        line (first lines)
        pline (second lines)]
    (if (nil? pline) ""
      (or (re-subs #"^\s*" pline) ""))))

(defn clojure-comment? [line]
  (re-test #"^\s*;" line))

(defn clojure-not-blank-or-comment? [line]
  (not (or (rblank? line) (clojure-comment? line))))

(defn clojure-get-indent[line]
  (cond 
    (= 1 (count (.trim line)))
    1
    (not (= (char-at line 0) \())
    1
    (re-test #"^\(\s*[^,\s]+[\s,]+[^,\s]+" line)
    (let [w (re-subs #"[^\s\[\{\(]+"
                     (subr line 1 (count line)))]
      (if (contains? indent-tab-size (str w))
        2
        (-> w count (+ 2))))
    :else 2))

;find outer scope and align by start bracket
(defn clojure-indent
  "Indent by brace parsing"
  [r pos]
  (let [[a b] (pos-line r pos)]
    (cond 
      (zero? a)
      ""
      (clojure-comment? (subr r a b))
      (auto-indent r pos)
      :else (let [tmp (reduce 
                        (fn[stack [a _]]
                          (let [ch (char-at r a)]
                            (if (and (contains? left-braces ch) (empty? stack))
                              (reduced a)
                              (if (= (peek stack) (all-braces ch))
                                (pop stack)
                                (conj stack ch))))) nil (pos-re-seq- r (dec a) re-braces))
                  mpos (if (number? tmp) tmp nil)]
              (if (nil? mpos)
                ""
                (let [ch (char-at r mpos)
                      [a b] (pos-line r mpos)
                      cnt (- mpos a)]
                  (repeat-space 
                    (+ (- mpos a) 
                       (clojure-get-indent (subr r mpos b))))))))))

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
        ;_ (println (str "[" line "]"))
        pline (or (first lines) "")
        ;_ (println (str "[" pline "]"))
        pindent (re-subs #"^\s*" pline)
        pbrace? (re-test #"[\{]\s*$" pline)
        brace? (re-test #"^\s*\}" line)]
    (cond (clang-comment? line)
          (auto-indent r pos)
          (empty? pline)
          ""
          (and (not pbrace?) brace?)
          (if (empty? pindent) "" (subr pindent 1))
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

(defn buf-indent-line[buf pos]
  (let [r (buf :str)
        indent ((-> buf :language :fn-indent) r pos)
        a (pos-line-first r pos)
        b (pos-line-start r pos)]
    ;(println a b)
    (buf-replace buf a b indent)))

(defn buf-indent-lines 
  "indent from cursor row to row both inclusive. Only indent not blank line."
  [buf [a b]]
  (let [r (buf :str)
        pos (buf :pos)
        lines (pos-lines-seq+ r a b)]
    (line-start
      (reduce
        #(buf-indent-line %1 (first %2)) 
        (buf-set-pos buf (-> lines first first))
        (filter #(not (rblank? r %)) lines)))))

(defn buf-indent-current-line
  [buf]
  (buf-indent-line buf (buf :pos)))

(def language-indents 
  {"Clojure" {:fn-indent clojure-indent}
   "JavaScript" {:fn-indent clang-indent
          :indent-triggers #"}"}
   "CSS" {:fn-indent clang-indent
           :indent-triggers #"}"}
   "XML" {:fn-indent auto-indent}
   :else {:fn-indent auto-indent}})

(defonce ^{:private true} listen-new-buffer
  (listen :new-buffer
          (fn[buf]
            (update-in buf [:language] 
                       merge (get language-indents 
                                  (-> buf :language :name) 
                                  (language-indents :else))))))
