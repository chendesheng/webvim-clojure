(ns webvim.buffer
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [clojure.java.io :as io])
  (:use clojure.pprint
        (clojure [string :only (join split)])
        webvim.autocompl
        webvim.global))

;global registers. Don't access this directly, always access buffer's :registers
(defonce registers (atom {}))

(def re-word-start #"(?<=\W)\w|(?<=[\s\w])[^\s\w]")
(def re-word-start-line-start #"^\S|(?<=\W)\w|(?<=[\s\w])[^\s\w]")
(def re-word-start-back #"(?<=\W)\w|(?<=[\s\w])[^\s\w]|^\S")

(def re-WORD-start #"(?<=\s)\S")
(def re-WORD-start-line-start #"(?=^\S)|(?<=\s)\S")
(def re-WORD-start-back #"(?<=\s)\S|(?=^\S)")

(def re-word-end #"(?=\S$)|(?=\w\W)|(?=[^\s\w][\s\w])")
(def re-WORD-end #"(?=\S$)|(?=\S\s)")
;
(defn split-lines-all 
  "Split by \\n and keep \\n. Always has a extra empty string after last \\n.
=> (split-lines-all \"\")
[\"\"]"
  [txt]
  (split txt #"(?<=\n)" -1))

(defn cursor-inc-col [cursor]
  (update-in cursor [:col] inc))

(defn inc-col [b]
  (update-in b [:cursor] cursor-inc-col))

(defn dec-col [b]
  (if (pos? (-> b :cursor :col))
    (update-in b [:cursor :col] dec)
    b))

(defn buffer-list-save
  "Generate buffer id (increase from 1) and add to buffer-list"
  [b]
  (let [id (swap! gen-buf-id inc)
        b1 (assoc b :id id)]
    (reset! buffer-list (assoc @buffer-list id b1))
    b1))

(defn create-buf[bufname txt]
  (let [b {:name bufname
           ;Each line is a standard java String
           :lines (split-lines-all txt)
           ;row, col, lastcol, viewport row (row from top of current viewport)
           :cursor {:row 0 :col 0 :lastcol 0 :vprow 0}
           ;For client display matched braces: [{:row :col} {:row :col}]
           ;If any of them equal to :cursor draw cursor only
           ;TODO set initial value
           :braces nil
           ;saved cursor when insert begins, for undo/redo function only
           :last-cursor nil
           ;:type =0 visual =1 visual line =2 visual block
           ;:ranges is a vector of point pairs (unordered): [{:row :col} {:row :col}]. Always contains even number of points. Right end is exclusive.
           :visual {:type 0 :ranges []}
           ;=0 nomral mode =1 insert mode =2 ex mode =3 visual mode
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
                       :suggestions-inex 0}}]

    ;The undo/redo function takes advantage of clojure's persistent data structure, just save everything we needs. Consider change to save keystrokes if memory usage is too high.
    ;Each history item holds two cursors: one is cursor pos when edit begins and the other is when edit ends. Perform undo action will recover cursor to cursor-begin and perform redo action recover cursor to cursor-end. 
    (-> b
        (assoc :history {:items [{:lines (:lines b) 
                                  ;The initial version doesn't need cursor-begin
                                  :cursor-begin nil 
                                  :cursor-end (:cursor b)}]
                         :version 0})
        autocompl-words-parse-buffer
        ;cache whole txt for searching, use :lines check if same version
        (assoc :txt-cache {:lines (:lines b)  :txt txt}))))

(defn open-file
  "Create buffer from a file by slurp."
  [f]
  (create-buf f (slurp f)))

(defn history-peek[b]
  ((-> b :history :items) (-> b :history :version)))

(defn buf-change-cursor-col[b col]
  (-> b
      (assoc-in [:cursor :col] col)
      (assoc-in [:cursor :lastcol] col)))

(defn buf-refresh-txt-cache
  "Call this function concate whole buffer into a single string and save to :txt-cache"
  [b]
  (if (= (:lines b) (-> b :txt-cache :lines))
    b
    (let [txt (join (:lines b))]
      (assoc b :txt-cache {:lines (:lines b) :txt txt}))))

;(defn history-changed?[b]
;  (let [item (history-peek b)]
;    (or (zero? item) (not (= (:lines b) (:lines item))))))

(defn buf-save-cursor
  "save cursor position when start insertion mode"
  [b]
  (assoc b :last-cursor (:cursor b)))

(defn history-save
  "push state into history, only :lines and :cursor is needed"
  [b]
  (let [item (history-peek b)]
    (if (not (= (:lines b) (:lines item)))
      (let [version (-> b :history :version)
            newversion (inc version)
            items (subvec (-> b :history :items) 0 newversion)]
        (dissoc (assoc b :history {:items (conj items
                                                {:lines (:lines b) 
                                                 :cursor-begin (:last-cursor b)
                                                 :cursor-end (:cursor b)})
                                   :version newversion}) :last-cursor))
      b)))

;(history-save {:history {:items [] :version 0} :lines [] :cursor {}}) 
;(history-save {:history {:items [{:lines [] :cursor {}}] :version 1} :lines ["aaa"] :cursor {}}) 

(defn history-undo[b]
  (if (pos? (-> b :history :version))
    (let [version (-> b :history :version)
          olditem ((-> b :history :items) version)
          newversion (dec version)
          item ((-> b :history :items) newversion)]
      (merge b {:cursor (:cursor-begin olditem)
                :lines (:lines item)
                :history 
                {:items (-> b :history :items) 
                 :version newversion}}))
    b))

(defn history-redo[b]
  (if (< (-> b :history :version) (-> b :history :items count dec))
    (let [newversion (-> b :history :version inc)
          item ((-> b :history :items) newversion)]
      (merge b {:cursor (:cursor-end item)
                :lines (:lines item)
                :history 
                {:items (-> b :history :items) 
                 :version newversion}}))
    b))

(defn col-count
  "Take row number return count length of a row."
  [b row]
  (count ((:lines b) row)))

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

(defn cursor-sort [cur1 cur2]
  (let [{r1 :row c1 :col} cur1
        {r2 :row c2 :col} cur2]
        (if (or (< r1 r2) (and (= r1 r2) (<= c1 c2)))
          [cur1 cur2]
          [cur2 cur1])))

(defn bound-range[v s e]
  (cond (<= v s) s
        (>= v e) e
        :else v))

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
              :cursor (merge cursor 
                             {:lastcol (:col cursor)
                              :vprow (-> (:row cursor)
                                         (- (:row cur1)) 
                                         (+ (:vprow cur1))
                                         (bound-range 0 (-> @window :viewport :h dec)))})})))

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

(defn buf-match-brace[b pt]
  (let [brace (buf-char-at b pt)
        m (all-braces brace)
        lines (:lines b)
        left? (contains? left-braces brace)]
    (if (nil? m) nil
      (if left?
        (loop[cnt 0
              row (:row pt)
              col (:col pt)]
          (if (< row (-> lines count)) ;EOF
            (let [ch (lines-char-at lines row col)
                  ncnt (cond (= brace ch)
                             (inc cnt)
                             (= m ch)
                             (dec cnt)
                             :else cnt)]
              (if (zero? ncnt) ;find match
                {:row row :col col}
                (let [[nrow ncol] (if (< col (-> row lines count dec))
                                    [row (inc col)]
                                    [(inc row) 0])]
                  (recur ncnt nrow ncol))))
            nil))
        (loop[cnt 0
              row (:row pt)
              col (:col pt)]
          (if (< row 0) ;EOF
            nil
            (let [ch (lines-char-at lines row col)
                  ncnt (cond (= brace ch)
                             (inc cnt)
                             (= m ch)
                             (dec cnt)
                             :else cnt)]
              (if (zero? ncnt) ;find match
                {:row row :col col}
                (let [[nrow ncol] (if (> col 0)
                                    [row (dec col)]
                                    [(dec row) (if (not (pos? row))
                                                 0
                                                 (-> (dec row) lines count dec))])]
                  (recur ncnt nrow ncol))))))))))

(defn line-next-re
  "Move to next charactor match by re." 
  [line col re]
  (let [subline (subs line col)
        m (re-matcher re subline)]
    (if (.find m)
      [(-> m .start (+ col)) (-> m .end (+ col))]
      nil)))

(defn buf-line[b row]
  (-> b :lines (get row)))

(defn cursor-line-end
  "The \"$\" motion"
  [b]
  (let [col (-> b :lines (get (-> b :cursor :row)) count dec)
        col1 (if (neg? col) 0 col)]
      (buf-change-cursor-col b col1)))

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

(defn line-back-re
  "Move to back charactor match by re. col=-1 means match whole line" 
  [line col re]
  (let [subline (if (= -1 col)
                  line
                  (subs line 0 col))
        m (re-matcher re subline)]
    (if (.find m)
      (loop [m1 m]
        (let [matched [(.start m1) (.end m1)]]
          (if (.find m1)
            (recur m1)
            matched)))
      nil)))

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
  (let [{row :row col :col vprow :vprow} (:cursor b)] 
    (cond 
      (and (= col 0) (> row 0))
      (buf-replace b (merge (:cursor b) 
                            {:row (dec row) :col (dec (col-count b (dec row))) :vprow (dec vprow)})
                   "" false)
      (> col 0)
      (buf-replace b (assoc (:cursor b) :col (dec col))
                   "" false)
      :else b)))


(defn calc-col [b row col lastcol]
  "set col to lastcol if length is avaliable"
  (let [cnt (col-count b row)]
    (cond 
      (> cnt lastcol) lastcol
      (< cnt 1) 0
      :else (dec cnt))))

;(calc-col test-buf 4 30 30)

(defn cursor-to-point [{row :row col :col}]
  {:row row :col col})

(defn cursor-move-start
  "Move to beginning of a buffer"
  [b]
  (assoc b :cursor {:row 0 :col 0 :lastcol 0 :vprow 0}))

(defn cursor-move-end
  "Move to first char of last line"
  [b]
  (assoc b :cursor
         {:row (-> b :lines count dec dec) :col 0 :lastcol 0 :vprow (-> @window :viewport :h dec)}))

(defn lines-next-re[{lines :lines cursor :cursor} re re-line-start]
  (loop [row (:row cursor)
         col (:col cursor)
         re-current re]
    (if (< row (count lines))
      (let [line (lines row)
            matched (line-next-re line col re-current)]
        (if (not matched)
          (recur (inc row) 0 re-line-start)
          [[{:row row :col (matched 0)} {:row row :col (matched 1)}]
           {:row row
            :vprow (-> row 
                       (- (:row cursor)) 
                       (+ (:vprow cursor))
                       (bound-range 0 (-> @window :viewport :h dec)))
            :col (matched 0)
            :lastcol (matched 0)}]))
      [nil cursor])))

(defn lines-back-re[{lines :lines cursor :cursor} re]
  (loop [row (:row cursor)
         col (:col cursor)]
    (if (>= row 0)
      (let [line (lines row)
            matched (line-back-re line col re)]
        (if (not matched)
          (recur (dec row) -1)
          [[{:row row :col (matched 0)} {:row row :col (matched 1)}]
           {:row row
            :vprow (-> row 
                       (- (:row cursor)) 
                       (+ (:vprow cursor))
                       (bound-range 0 (-> @window :viewport :h dec)))
            :col (matched 0)
            :lastcol (matched 0)}]))
      [nil cursor])))

(defn cursor-next-re
  "Match re line by line (re not match multiple lines), don't change cursor if nothing is found"
  [b re re-line-start]
  (let [[matched newcursor] (lines-next-re b re re-line-start)]
    (if (nil? matched)
      b
      (assoc b :cursor newcursor))))

(defn cursor-back-re
  "Match re line by line in reverse"
  [b re]
  (loop [row (-> b :cursor :row)
         col (-> b :cursor :col)]
    (if (>= row 0)
      (let [line (-> b :lines (get row))
            matched (line-back-re line col re)]
        (if (nil? matched)
          (recur (dec row) -1)
          (-> b 
              (assoc-in [:cursor :row] row)
              (assoc-in [:cursor :vprow] (-> (-> b :cursor :vprow)
                                             (- (-> b :cursor :row)) 
                                             (+ row)
                                             (bound-range 0 (-> @window :viewport :h dec))))
              (assoc-in [:cursor :col] (matched 0))
              (assoc-in [:cursor :lastcol] (matched 0)))))
      b)))

(defn cursor-back-word
  "The \"b\" motion."
  [b]
  (cursor-back-re b re-word-start-back))

(defn cursor-back-WORD
  "The \"B\" motion."
  [b]
  (cursor-back-re b re-WORD-start-back))


(defn cursor-next-word
  "The \"w\" motion. Difference from vim's \"w\" is this motion will skip empty lines"
  [b]
  (cursor-next-re b 
                  re-word-start
                  re-word-start-line-start))

(defn cursor-next-WORD
  "The \"W\" motion"
  [b]
  (cursor-next-re b 
                  re-WORD-start
                  re-WORD-start-line-start))

(defn cursor-word-end
  "The \"e\" motion."
  [b]
  (-> b 
      inc-col
      (cursor-next-re re-word-end re-word-end)))

(defn cursor-WORD-end
  "The \"E\" motion."
  [b]
  (-> b 
      inc-col
      (cursor-next-re re-WORD-end re-WORD-end)))

(defn cursor-line-first
  "The \"0\" motion"
  [b]
  (if (-> b :cursor :col zero?)
    b
    (buf-change-cursor-col b 0)))

(defn cursor-line-start
  "The \"^\" motion"
  [b]
  (let [line (-> b :lines (get (-> b :cursor :row)))
        [start end] (if (= 1 (count line))
                        [nil nil]
                        (line-next-re line 0 #"^\S|(?<=\s)\S"))]
    (if (nil? start)
      (buf-change-cursor-col b start)
      b)))

(defn cursor-match-brace[b]
  (let [b1 (cursor-next-re b #"[\(\[\{\)\]\}]" #"[\(\[\{\)\]\}]")]
    (buf-match-brace b1 (:cursor b1))))

(defn quote-pattern[ch]
  (java.util.regex.Pattern/quote ch))

(defn cursor-next-char
  [b ch]
  (let [line (-> b :lines (get (-> b :cursor :row)))
        col (-> b :cursor :col inc)
        re (str "(?=" (quote-pattern ch) ")")
        [matched newcol] (line-next-re line col (re-pattern re))]
    (if matched
      (buf-change-cursor-col b newcol)
      b)))

(defn cursor-back-char
  [b ch]
  (let [line (-> b :lines (get (-> b :cursor :row)))
        col (-> b :cursor :col)
        re (str "(?=" (quote-pattern ch) ")")
        matched (line-back-re line col (re-pattern re))]
    (if (not (nil? matched))
      (buf-change-cursor-col b (matched 0))
      b)))

(defn cursor-move-char
  "Move one character. Direction 0,1,2,3 -> left,right,up,down
    In normal mode the cursor should never go to \n"
  [b direction]
  (let [{row :row col :col lastcol :lastcol vprow :vprow } (:cursor b)]
    (assoc b :cursor 
           (merge (:cursor b) 
                  (cond 
                    ;move left
                    (and (= direction 0) (pos? col))
                    (let [c (dec col)]
                      {:col c :lastcol c})

                    ;move right
                    (and (= direction 1) (> (col-count b row) (inc col)))
                    (let [c (inc col)]
                      {:col c :lastcol c})

                    ;move up
                    (and (= direction 2) (pos? row))
                    (let [newvprow 
                          (if (-> vprow dec neg?)
                            0
                            (dec vprow))]
                      {:row (dec row) :col (calc-col b (dec row) col lastcol) :vprow newvprow})

                    ;move down
                    (and (= direction 3) (> (-> b :lines count dec) (inc row)))
                    (let [newvprow 
                          (if (< vprow (-> @window :viewport :h dec))
                            (inc vprow)
                            (-> @window :viewport :h dec))]
                      {:row (inc row) :col (calc-col b (inc row) col lastcol) :vprow newvprow})

                    :else (:cursor b))))))

(defn cursor-simple-next-str
  "no wrap to beginning, no highlights"
  [b s]
  (let [re (re-pattern s)
        [matched newcur] (lines-next-re (inc-col b) re re)]
    (if (nil? matched) 
      b
      (assoc b :cursor newcur))))

(defn cursor-simple-back-str
  [b s]
  (let [re (re-pattern s)
        [matched newcur] (lines-back-re b re)]
    (if (nil? matched) 
      b
      (assoc b :cursor newcur))))

(defn cursor-next-str
  "The \"n\" command"
  [b s]
  (if (empty? s)
    b
    (let [re (re-pattern s)
          [matched newcur] (lines-next-re (inc-col b) re re)]
      (if (nil? matched) ;not found, wrap back and searh again
        (let [[matched2 newcur2] (lines-next-re (cursor-move-start b) re re)]
          (if matched2
            (-> b 
                (assoc :highlights matched2)
                (assoc :cursor newcur2))
            b))
        (-> b 
            (assoc :highlights matched)
            (assoc :cursor newcur))))))

(defn cursor-back-str
  "The \"N\" motion"
  [b s]
  (if (empty? s)
    b
    (let [re (re-pattern s)
          [matched newcur] (lines-back-re b re)]
      (if (nil? matched) ;not found, wrap back and searh again
        (let [[matched2 newcur2] (lines-back-re (cursor-move-end b) re)]
          (if matched2
            (-> b 
                (assoc :highlights matched2)
                (assoc :cursor newcur2))
            b))
        (-> b 
            (assoc :highlights matched)
            (assoc :cursor newcur))))))

(defn first-nonspace-pos
  "Return index of first non-space char"
  [line]
  (let [m (re-matcher #"\S" line)]
    (if (.find m)
      (.start m)
      0)))

(defn round-to-zero
  "(round-to-zero -9.1) = -9; (round-to-zero 9.1) = 9"
  [i]
  (if (> i 0)
    (int i)
    (- (int (- i)))))

(defn cursor-move-viewport
  "Jump cursor by viewport height, deps to window's :viewport, keep cursor's viewport row unchanged."
  [b factor]
  (let [d (round-to-zero (* (:h (:viewport @window)) factor))
        row (+ (-> b :cursor :row) d)
        newrow (cond 
                 (< row 0)
                 0

                 (>= row (-> b :lines count))
                 (-> b :lines count dec)

                 :else
                 row)
        newcol (first-nonspace-pos ((:lines b) newrow))]
    (assoc b :cursor 
           (merge (:cursor b) 
                  {:row newrow :col newcol :lastcol newcol}))))


(defn cursor-center-viewport[b]
  (assoc b :cursor 
         (merge (:cursor b) 
                {:vprow (int (/ (-> @window :viewport :h) 2))})))


(defn write-buffer
  "Write buffer to disk. This operation MUST be atomic."
  [b]
  (try 
    (let [lines (:lines b)
          f (:name b)
          tmp (str (fs/tmpdir) (fs/base-name f))]
      (if (fs/exists? tmp)
        (fs/delete tmp))
      (fs/create (fs/file tmp))
      (with-open [wrtr (io/writer (fs/file tmp))]
        (doseq [line lines]
          (.write wrtr line)))
      ;TODO handle line break
      ;TODO fsync before rename
      ;TODO Windows?
      (fs/rename tmp f)
      (assoc b :message (str "\"" f "\" " (count lines) "L written")))
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
        vprow (-> b :cursor :vprow)
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
        (assoc :cursor {:row (inc row) :col 0 :lastcol 0 
                        :vprow (-> b :cursor :vprow inc (bound-range 0 (-> @window :viewport :h dec)))}))))

(defn trim-left-space[line]
  (.replaceAll line "^ +" ""))

(defn buf-indent-new-line
  "auto indent, append same space as previous line"
  [b]
  (let [lines (b :lines)
        row (-> b :cursor :row)
        line (lines row)]
    (if (zero? row)
      b
      (let [pline (lines (dec row))
            prefix (re-find #"( +)[^ ]" pline)
            line (lines row)]
        (if (nil? prefix)
          b
          (let [s (prefix 1)] 
            (-> b
              (assoc :lines (assoc lines row (str s (trim-left-space line))))
              (buf-change-cursor-col (.length s)))))))))

;(buf-indent-new-line {:lines ["  hello" "\n" ""] :cursor {:row 1 :col 0 :lastcol 0 :vprow 0}})

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
