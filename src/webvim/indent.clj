(ns webvim.indent
  (:use clojure.pprint
        (clojure [string :only (join split blank?)])
        webvim.text
        webvim.global))

;trim leading spaces but not '\n'
;only use in indent function
(defn trim-left-space[line]
  (.replaceAll line "^[ \t]+" ""))

(def indent-tab-size #{"def" "defn" "if" "fn" "let" "cond" "loop"})

(defn get-indent[line]
  (cond 
    (= 1 (count (.trim line)))
    1
    (re-test #"\(\s*[^,\s]+[\s,]+[^,\s]+" line)
    (let [w (re-subs #"[^\s\[\{\(]+" (trim-left-space (text-subs line 1)))]
      (if (contains? indent-tab-size w)
        2
        (let [m (re-matcher #"\(\s*[^,\s]+[\s,]*([^,\s]+)" line)]
          (.find m)
          (.start m 1))))
    :else 2))

(defn clojure-indent
  "Indent by brace parsing"
  [lines row]
  (if (zero? row)
    ""
    (loop [current (dec row)
           braces []]
      ;(pprint2 braces "braces:")
      (cond 
        (neg? current)
        ""
        (blank? (lines current))
        (recur (dec current) braces)
        :else
        (let [line (lines current)
              ;_ (println line)
              nbraces (reduce 
                        (fn[stack pt]
                          (let [ch (-> pt :group first) ]
                            ;(println "ch:" ch)
                            (if (and (not (empty? stack))
                                     (= (-> stack last :group first) (all-braces ch)))
                              (pop stack)
                              (conj stack pt))))
                        ;from right to left
                        braces (reverse (re-seq-pos re-braces line 0)))]
          ;(println "current:" current)
          ;(pprint2 nbraces "nbraces:")
          ;(println "line:")
          ;(println line)
          (cond (empty? nbraces)
                (repeat-space (count-left-spaces line))
                (contains? left-braces (-> nbraces first :group first))
                (let [m (first nbraces)
                      ch (-> m :group first)
                      start (m :start)]
                  ;(println "start:" start)
                  (if (= ch \()
                    (repeat-space (+ (get-indent (text-subs line start)) start))
                    (repeat-space (inc (m :start)))))
                :else (recur (dec current) nbraces)))))))

(defn auto-indent 
  [s pos]
  (let [lines (filter #(-> % text-blank? not)
                      (ranges-to-texts s (lines-reverse s pos)))
        line (first lines)
        pline (second lines)]
    (if (nil? pline) ""
      (or (re-subs #"^\s*" pline) ""))))

(defn c-comment? [line]
  (re-test #"^\s*//" line))

(defn not-blank-or-comment? [line]
  (not (or (text-blank? line) (c-comment? line))))

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
        lines (filter not-blank-or-comment?
                      (ranges-to-texts s ranges))
        _ (println (str "[" line "]"))
        pline (or (first lines) "")
        _ (println (str "[" pline "]"))
        pindent (re-subs #"^\s*" pline)
        pbrace? (re-test #"[\{]\s*$" pline)
        brace? (re-test #"^\s*\}" line)]
    (cond (c-comment? line)
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
      (if (< (t :pos) a) t
        (let [t1 (buf-indent-line t a)
              s1 (t1 :str)]
          (recur 
            t1
            ;use a as start, a doesn't change when indent current line
            (pos-next-line s1 a #(not (range-blank? s1 %)))))))))

(defn buf-indent-current-line
  [b]
  (buf-indent-line b (b :pos)))
