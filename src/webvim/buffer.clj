(ns webvim.buffer
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [clojure.java.io :as io])
  (:use clojure.pprint
        (clojure [string :only (join split blank?)])
        webvim.autocompl
        webvim.history
        webvim.cursor
        webvim.global))

(def re-braces #"\(|\[|\{|\}|\]|\)")
(declare clojure-indent)
(declare auto-indent)
(declare clang-indent)

;trim leading spaces but not '\n'
;only use in indent function
(defn trim-left-space[line]
  (.replaceAll line "^[ \t]+" ""))

(defn split-lines-all 
  "Split by \\n and keep \\n. Always has a extra empty string after last \\n.
=> (split-lines-all \"\")
[\"\"]"
  [txt]
  (split txt #"(?<=\n)" -1))

(defn buffer-list-save
  "Generate buffer id (increase from 1) and add to buffer-list"
  [b]
  (let [id (swap! gen-buf-id inc)
        b1 (assoc b :id id)]
    (reset! buffer-list (assoc @buffer-list id b1))
    b1))

(defn create-buf[bufname txt]
  (let [languages {".clj" {:name "Clojure"
                           :fn-indent clojure-indent}
                   ".js" {:name "JavaScript"
                          :fn-indent clang-indent
                          :indent-triggers #"}"}
                   ".css" {:name "CSS"
                           :fn-indent clang-indent
                           :indent-triggers #"}"}
                   ".html" {:name "XML"
                           :fn-indent auto-indent}
                   :else {:name "Plain Text"
                          :fn-indent auto-indent}}
        ext (re-find #"\.\w+$" bufname)
        b {:name bufname
           ;Each line is a standard java String
           :lines (split-lines-all txt)
           ;row, col, lastcol, viewport row (row from top of current viewport)
           :cursor {:row 0 :col 0 :lastcol 0}
           ;For client display matched braces: [{:row :col} {:row :col}]
           ;If any of them equal to :cursor draw cursor only
           ;TODO set initial value
           :braces nil
           ;saved cursor when insert begins, for undo/redo function only
           :last-cursor nil
           ;save :lines object when write buffer to disk, check if lines unsaved
           :last-saved-lines nil
           ;:type =0 visual =1 visual line =2 visual block
           ;:ranges is a vector of point pairs (unordered): [{:row :col} {:row :col}]. Always contains even number of points. Both end are inclusive
           :visual {:type 0 :ranges []}
           ;=0 normal mode =1 insert mode =2 ex mode =3 visual mode
           :mode 0
           ;ongoing ex command
           :ex ""
           ;ongoing command keys, display beside "-- MODE --" prompt. Only save keys trigger next keymap, right before :enter function is called.
           :keys []
           ;send key to this channel when editting this buffer
           :chan-in (async/chan)
           ;get result from this channel after send key to :chan-in
           :chan-out (async/chan)

           :macro {:recording-keys nil
                   ;which register will macro save to
                   :register ""}
           ;Highlight texts, for hlsearch. Same format with visual ranges.
           :highlights []

           ;Local registers, atom. Set init value to global registers so it can share cross buffers.
           ;Use different registers in macro replaying to avoid side effect.
           :registers registers

           :autocompl {:words nil
                       ;empty suggestions means don't display it
                       ;every input handled in insertion mode should check if :suggestion is nil.
                       ;  if it is not nil then continue narrow down suggestions
                       ;ctrl+n, ctrl+p will calculate full suggestions if it is nil
                       :suggestions nil
                       ;0 means selection nothing (don't highlight any suggestion item)
                       ;> 0 means highlight the nth suggestion item
                       :suggestions-index 0}
           ;screen scrollTop row number, update after every key press
           :scroll-top 0
           ;programming language specific configs
           ;detect language by file ext name
           ;TODO detect language by file content
           :language (get languages ext (languages :else))}]
    (pprint (b :language))

    (autocompl-words-parse-lines (b :lines))
    ;The undo/redo function takes advantage of clojure's persistent data structure, just save everything we needs. TODO save changes
    ;Each history item holds two cursors: one is cursor pos when edit begins and the other is when edit ends. Perform undo action will recover cursor to cursor-begin and perform redo action recover cursor to cursor-end. 
    (-> b
        (assoc :last-saved-lines (b :lines))
        (assoc :history {:items [{:lines (:lines b) 
                                  ;The initial version doesn't need cursor-begin
                                  :cursor-begin nil 
                                  :cursor-end (:cursor b)}]
                         :version 0})
        ;cache whole txt for searching, use :lines check if same version
        (assoc :txt-cache {:lines (:lines b)  :txt txt}))))

(defn open-file
  "Create buffer from a file by slurp."
  [f]
  (create-buf f (slurp f)))

(defn buf-refresh-txt-cache
  "Call this function concate whole buffer into a single string and save to :txt-cache"
  [b]
  (if (= (:lines b) (-> b :txt-cache :lines))
    b
    (let [txt (join (:lines b))]
      (assoc b :txt-cache {:lines (:lines b) :txt txt}))))

(defn buf-save-cursor
  "save cursor position when start insertion mode"
  [b]
  (assoc b :last-cursor (:cursor b)))

(defn pt-push
  "set pt to next BOL if it is EOL"
  [pt lines]
  (let [{r :row c :col} pt
        c_r (count (lines r))]
    (if (= c c_r)
      (if (>= r (dec (count lines)))
        pt
        {:row (-> pt :row inc) :col 0})
      pt)))

(defn pt-pull[pt lines]
  (cond
    (and (zero? (pt :row)) (zero? (pt :col)))
    pt
    (zero? (pt :col)) 
    (let [r1 (-> pt :row dec)]
      {:row r1 :col (count (lines r1))})
    :else
    pt))

;TODO: fix buffer insert after line break
(defn replace-range 
  "Core operation of buffer lines manipulation. 
  [r1 c1] must before [r2 c2].
  It's exclusive not include [r2 c2].
  The lines argument must contains at least one item (could be an empty string)"
  [lines pt1 pt2 txt]
  ;__________
  ;__________ ;prefix 
  ;____...... ;l1
  ;..........
  ;..________ ;l2
  ;__________ ;suffix
  (let [{r1 :row c1 :col} (pt-push pt1 lines)
        {r2 :row c2 :col} (pt-push pt2 lines)
        prefix (subvec lines 0 r1)
        [_ & suffix] (subvec lines r2)
        l1 (subs (lines r1) 0 c1)
        l2 (subs (lines r2) c2)
        txt-lines (split-lines-all txt)
        middle (-> txt-lines
                   (update 0 #(str l1 %))
                   (update (dec (count txt-lines)) #(str % l2)))
        newcol (- (count (last middle)) (count l2))
        newrow (+ r1 (dec (count txt-lines)))
        newlines (vec (concat prefix middle suffix))
        newcur (if (= newrow (dec (count newlines)))
                 (pt-pull {:row newrow
                           :col newcol} newlines)
                 {:row newrow
                  :col newcol})]

    ;(print "prefix:")
    ;(pprint prefix)
    ;(println (str "l1:" l1))
    ;(println (str "l2:" l2))
    ;(print "middle:")
    ;(pprint middle)
    ;(print "suffix:")
    ;(pprint suffix)
    {:lines newlines
     :cursor newcur}))

(defn buf-replace 
  "It's exclusive (not include cur)"
  [b cur txt inclusive]
  (let [[cur1 cur2] (cursor-sort (:cursor b) cur)
        ;_ (print "cur1:")
        ;_ (pprint cur1)
        ;_ (print "cur2:")
        ;_ (pprint cur2)
        cur3 (if inclusive (cursor-inc-col cur2) cur2)
        {lines :lines cursor :cursor} (replace-range (:lines b) cur1 cur3 txt)]
    (merge b {:lines lines
              :cursor (assoc cursor :lastcol (:col cursor))})))

(defn char-under-cursor[{lines :lines cursor :cursor}]
  (let [{row :row col :col} cursor]
    (.charAt (lines row) col)))

(def left-braces #{\( \[ \{})
(def right-braces #{\) \] \}})
(def all-braces {\( \) \) \( \[ \] \] \[ \{ \} \} \{})

(defn buf-char-at[b {row :row col :col}]
  (let [line (-> b :lines (get row))]
    (if (< col (count line))
      (.charAt line col)
      0)))

(defn lines-char-at[lines row col]
  (let [line (lines row)]
    (if (< col (count line))
      (.charAt line col)
      0)))

;http://stackoverflow.com/questions/21191045/get-string-indices-from-the-result-of-re-seq
(defn re-seq-pos [pattern string start] 
  (let [m (re-matcher pattern (subs string start))] 
    ((fn step [] 
      (when (. m find) 
        (cons {:start (+ (. m start) start) :end (+ (. m end) start) :group (. m group)} 
          (lazy-seq (step))))))))

(defn buf-match-brace[b pt]
  (let [brace (buf-char-at b pt)
        m (all-braces brace)
        lines (:lines b)
        left? (contains? left-braces brace)
        re (re-pattern (str  (quote-pattern brace) "|" (quote-pattern m)))]
    (if (nil? m) nil
      (if left?
        (loop[cnt 0
              {row :row col :col} pt]
          ;(println "row:" row ",col:" col)
          (let [line (lines row)
                matches (re-seq-pos re line col)
                [ncnt, col] (reduce 
                              (fn [[cnt, _] match]
                                (let [{start :start group :group} match
                                      ch (.charAt group 0)
                                      ncnt (if (= brace ch)
                                             (inc cnt)
                                             (dec cnt))]
                                  ;(println "start:" start ",ncnt:" ncnt, ",ch:" ch)
                                  (if (zero? ncnt) 
                                    (reduced [0, start])
                                    [ncnt, start])))
                              [cnt (-> line count dec)], matches)]
            ;(println ncnt)
            (cond (zero? ncnt)
                  {:row row :col col}
                  (>= (inc row) (count lines))
                  nil
                  :else
                  (recur ncnt {:row (inc row) :col 0}))))
        (loop[cnt 0
              row (pt :row)
              col (-> pt :col inc)]
          ;(println "right")
          ;(pprint pt)
          ;(println "row:" row ",col:" col)
          (let [line (lines row)
                subline (if (= col -1)
                          line
                          (subs line 0 col))
                matches (reverse (re-seq-pos re subline 0))
                [ncnt, col] (reduce 
                              (fn [[cnt, _] match]
                                (let [{start :start group :group} match
                                      ch (.charAt group 0)
                                      ncnt (if (= brace ch)
                                             (inc cnt)
                                             (dec cnt))]
                                  ;(println "start:" start ",ncnt:" ncnt, ",ch:" ch)
                                  (if (zero? ncnt) 
                                    (reduced [0, start])
                                    [ncnt, start])))
                              [cnt (-> line count dec)], matches)]
            ;(println ncnt)
            (cond (zero? ncnt)
                  {:row row :col col}
                  (zero? row) 
                  nil
                  :else
                  (recur ncnt (dec row) -1))))))))

(defn cursor-match-brace[b]
  (let [b1 (cursor-next-re b #"[\(\[\{\)\]\}]" #"[\(\[\{\)\]\}]")]
    (buf-match-brace b1 (:cursor b1))))

(defn buf-line[b row]
  (-> b :lines (get row)))

;for "dw" or "cw"
(defn buf-line-next-re
  "Not cross line, goto line end if not found"
  [b re]
  (let [{r :row c :col} (b :cursor)
        line (buf-line b r)
        [start end] (line-next-re line c re)]
    (if (nil? start)
      (cursor-line-end b)
      (buf-change-cursor-col b start))))

(defn buf-line-next-word[b]
  (buf-line-next-re b re-word-start))

(defn buf-line-next-WORD[b]
  (buf-line-next-re b re-WORD-start))

(defn word-under-cursor
  "Find next word end, then find word start back from that end."
  [lines {row :row col :col}]
  (let [line (lines row)
        matched (line-next-re line col re-word-end)
        end (if (not (nil? matched)) (inc (matched 0)) (count line))
        matched1 (line-back-re line end re-word-start)
        start (if (not (nil? matched1)) (matched1 0) 0)]
    [(subs line start end) start]))

(defn buf-insert 
  "Insert at cursor"
  [b txt]
  (buf-replace b (:cursor b) txt false))

(defn buf-delete
  "Delete cursor left char"
  [b]
  (let [{row :row col :col} (:cursor b)] 
    (cond 
      (and (= col 0) (> row 0))
      (buf-replace b (merge (:cursor b) 
                            {:row (dec row) :col (dec (col-count b (dec row)))})
                   "" false)
      (> col 0)
      (buf-replace b (assoc (:cursor b) :col (dec col))
                   "" false)
      :else b)))

(defn buf-delete-range[b]
  (let [[pt1 tmp] (apply cursor-sort (-> b :visual :ranges))
        pt2 (cursor-inc-col tmp)
        {lines :lines newcur :cursor} (replace-range (:lines b) pt1 pt2 "")]
    (-> b 
        (update-in [:cursor] merge newcur)
        (assoc-in [:visual :ranges] [])
        (assoc :lines lines)
        (assoc-in [:cursor :lastcol] (newcur :col)))))

(defn buf-delete-range2[b pt1 pt2]
  (let [[from to] (cursor-sort pt1 pt2)
	    {lines :lines newcur :cursor} (replace-range (:lines b) from to "")]
    (-> b 
        (update-in [:cursor] merge newcur)
        (assoc-in [:visual :ranges] [])
        (assoc :lines lines)
        (assoc-in [:cursor :lastcol] (newcur :col)))))

(defn buf-lines-unsaved?
  "if :lines unsaved after last save"
  [b]
  (not (= (b :lines) (b :last-saved-lines))))

;(calc-col test-buf 4 30 30)

;TODO make write atomic
(defn write-buffer
  [b]
  (try 
    (let [lines (:lines b)
          f (:name b)]
      (spit f (join lines))
      (-> b
          (assoc :message (str "\"" f "\" " (count lines) "L written"))
          (assoc :last-saved-lines (b :lines))))
    (catch Exception e 
      (println (.getMessage e))
      (.printStackTrace e)
      (let [err (str "caught exception: " (.getMessage e))]
        (assoc b :message err)))))

(defn buffer-append-keys[b keycode]
  (assoc b :keys (conj (:keys b) keycode)))

(defn buffer-reset-keys[b]
  (assoc b :keys []))

(defn buffer-word-before-cursor[{lines :lines cur :cursor}]
  (let [{row :row col :col} cur
        line (lines row)]
    (let [s (loop [i (dec col)]
              (if (neg? i)
                0
                (let [ch (int (.charAt line i))]
                  (if (or (<= (int \a) ch (int \z)) (<= (int \A) ch (int \Z)) (= ch (int \_)))
                    (recur (dec i))
                    (inc i)))))]
      (subs line s col))))

(defn buffer-replace-suggestion[b word]
  (let [subject (buffer-word-before-cursor b)
        {col :col} (:cursor b)]
    (buf-replace b (assoc (:cursor b) :col (- col (count subject))) word false)))

(defn buf-delete-line
  "Delete line under cursor and put cursor to next line"
  [b]
  (let [row (-> b :cursor :row)
        line (-> b :lines (get row))
        {lines :lines cursor :cursor} (replace-range (:lines b)
                                                     {:row row :col 0}
                                                     {:row row :col (col-count b row)}
                                                     "")]
    (-> b 
        (assoc :lines lines)
        (assoc-in [:cursor :row] (:row cursor))
        (assoc-in [:cursor :col] (:col cursor))
        cursor-line-first)))

(defn buf-insert-line-after
  "The \"o\" command"
  [b]
  (let [lines (:lines b)
        row (-> b :cursor :row)]
    (-> b
        (assoc :lines 
               (vec (concat
                      (subvec lines 0 (inc row))
                      ["\n"]
                      (subvec lines (inc row)))))
        (assoc :cursor {:row (inc row) :col 0 :lastcol 0}))))

(def indent-tab-size #{"def" "defn" "if" "fn" "let"})
(defn get-indent[line]
  (cond 
    (= 1 (count (.trim line)))
    1
    (re-test #"\(\s*[^,\s]+[\s,]+[^,\s]+" line)
    (let [w (re-find #"[^\s\[\{\(]+" (trim-left-space (subs line 1)))]
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
    0
    (loop [current (dec row)
           braces []]
      ;(pprint2 braces "braces:")
      (cond 
        (neg? row)
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
                    (repeat-space (+ (get-indent (subs line start)) start))
                    (repeat-space (inc (m :start)))))
                :else (recur (dec current) nbraces)))))))

(defn prev-non-blank-line[lines row]
  (loop [i (dec row)]
    (let [line (lines i)]
      (if (blank? line)
        (recur (dec i))
        i))))

(defn auto-indent [lines row]
  (if (zero? row)
    ""
    (let [line (lines row)
          prow (prev-non-blank-line lines row)
          pline (lines prow)]
      (or (re-find #"^\s*" pline) ""))))

(defn clang-indent [lines row]
  (if (zero? row)
    ""
    (let [line (lines row)
          prow (prev-non-blank-line lines row)
          pline (lines prow)
          pindent (or (re-find #"^\s*" pline) "")]
      (println "pindent:" (count pindent))
      (cond 
        (re-test #"^\s*\}\s*$" line)
        (if (empty? pindent) "" (subs pindent 1))
        (re-test #"[\{\)]\s*$" pline)
        (str pindent "\t")
        :else
        pindent))))

(defn buf-indent-line[b row]
  (let [indent ((-> b :language :fn-indent) (b :lines) row)
        line (buf-line b row)]
    (-> b
        (assoc-in [:lines row] (str indent (trim-left-space line))))))

(defn buf-indent-lines 
  "indent from cursor row to row both inclusive"
  [b row]
  (let [r (-> b :cursor :row)
        [r1 r2] (if (> r row) [row r] [r row])]
    ;(println "buf-indent-lines:" r1 r2)
    (loop[i r1
          b1 b]
      (cond 
        (> i r2) (-> b1 
                    (assoc-in [:cursor :row] r1)
                    (cursor-line-start))
        :else
        (recur (inc i) (buf-indent-line b1 i))))))

(defn buf-indent-current-line
  [b]
  (buf-indent-lines b (-> b :cursor :row)))

(defn buf-copy-range[b p1 p2 inclusive]
  (let [[{r1 :row c1 :col} cur2] (cursor-sort p1 p2)
        {r2 :row c2 :col} (if inclusive (cursor-inc-col cur2) cur2)
        res (loop [res []
                   r r1]
              (if (= r1 r2)
                (conj res (subs (buf-line b r) c1 c2))
                (cond 
                  (= r r2)
                  (conj res (subs (buf-line b r) 0 c2))
                  (= r r1)
                  (recur (conj res (subs (buf-line b r) c1)) (inc r))
                  :else
                  (recur (conj res (buf-line b r)) (inc r)))))]
    (join res)))

(defn buf-update-highlight-brace-pair[b pt]
  (let [pt2 (buf-match-brace b pt)]
    (if (nil? pt2)
      (dissoc b :braces)
      (assoc b :braces [{:row (:row pt) :col (:col pt)} pt2]))))

(defn buf-join-line
  "join current line and next line"
  [b]
  (let [{r :row c :col} (b :cursor)]
    (if (< r (-> b :lines count dec))
      (let [[start _] (line-next-re (buf-line b (inc r)) 0 #"^\S|(?<=\s)\S")
            col (if (nil? start)
                  (dec (col-count b (inc r)))
                  start)]
        (-> b
            cursor-line-end
            (buf-replace {:row (inc r) :col col} " " false)
            dec-col))
      b)))

(defn buf-bound-scroll-top
  "Change scroll top make cursor inside viewport"
  [b]
  (let [st (-> b :scroll-top)]
    (assoc b :scroll-top 
           (let [row (-> b :cursor :row)
                 h (-> @window :viewport :h)]
             (cond 
               (< row st) row
               (< row (+ st h)) st
               (neg? (-> row (- h) inc)) 0
               :else (-> row (- h) inc))))))
