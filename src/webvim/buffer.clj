(ns webvim.buffer
  (:require [me.raynes.fs :as fs]
            [clojure.java.io :as io])
  (:use clojure.pprint
        webvim.autocompl
        (clojure [string :only (join split)])))

(defn split-lines-all [txt]
  (split txt #"\r?\n" -1))

(defn create-buf[bufname txt]
  (let [b {:name bufname
           ;Each line is a standard java String
           :lines (split-lines-all txt)
           ;row, col, lastcol, viewport row (row from top of current viewport)
           :cursor {:row 0 :col 0 :lastcol 0 :vprow 0}
           ;saved cursor when insert begins, for undo/redo function only
           :last-cursor nil
           ;:type =0 visual =1 visual line =2 visual block
           ;:ranges is a vector of point pairs (unordered): [[row1 col1] [row2 col2]]. Alwasy contains even number of points. Both ends are inclusive.
           :visual {:type 0 :ranges []}
           ;=0 nomral mode =1 insert mode =2 ex mode =3 visual mode
           :mode 0
           ;ongoing ex command
           :ex ""
           ;ongoing command keys, display beside "-- MODE --" prompt
           :keys []

           :autocompl {:words nil
                       ;empty suggestions means don't display it
                       ;every input handled in insertion mode should check if :suggestion is nil.
                       ;  if it is not nil then continue narrow down suggestions
                       ;ctrl+n, ctrl+p will calculate full suggestions if it is nil
                       :suggestions nil
                       ;0 means selection nothing (don't highlight any suggestion item)
                       ;> 0 means highlight the nth suggestion item
                       :suggestions-inex 0}

           ;viewport size
           :viewport {:w 0 :h 0}}]
    ;TODO: put all server only states (like :viewport) into one map and don't send to client

    ;The undo/redo function takes advantage of clojure's persistent data structure, just save everything we needs. Consider change to save keystrokes if memory usage is too high.
    ;Each history item holds two cursors: one is cursor pos when edit begins and the other is when edit ends. Perform undo action will recover cursor to cursor-begin and perform redo action recover cursor to cursor-end. 
    (autocompl-parse-buffer 
      (assoc b :history {:items [{:lines (:lines b) 
                                  ;The initial version doesn't need cursor-begin
                                  :cursor-begin nil 
                                  :cursor-end (:cursor b)}]
                         :version 0}))))

(defn open-file[f]
  "Create buffer from a file"
  (create-buf f (slurp f)))

(defonce active-buffer (atom {}))

;only for testing on repl
(reset! active-buffer (open-file "testfile.clj"))

(defn history-peek[b]
  ((-> b :history :items) (-> b :history :version)))

(defn buf-refresh-txt-cache
  "Call this function concate whole buffer into a single string and save to :txt-cache"
  [b]
  (if (= (:lines b) (-> b :txt-cache :lines))
    b
    (let [txt (join "\n" (:lines b))]
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

(defn replace-range 
  "Core operation of buffer lines manipulation"
  [lines [{r1 :row c1 :col} {r2 :row c2 :col}] txt]
  (if (zero? (count lines))
    (if (pos? (count txt))
      (let [txt-lines (split-lines-all txt)
            newcol (count (last txt-lines))]
        {:lines txt-lines
         :cursor {:row (-> txt-lines count dec)
                  :col newcol}}))
    (let [prefix (subvec lines 0 r1)
          suffix (subvec lines (inc r2))
          l1 (lines r1)
          l2 (lines r2)
          txt-lines (split-lines-all txt)
          newcol (if (= 1 (count txt-lines))
                   (+ c1 (count (first txt-lines)))
                   (count (last txt-lines)))
          middle (update
                   (update txt-lines 0 #(str (subs l1 0 c1) %))
                   (dec (count txt-lines))
                   #(str % (subs l2 c2)))]
      {:lines (vec (concat prefix middle suffix))
       :cursor {:row (+ r1 (dec (count txt-lines))) 
                :col newcol}})))

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

(defn buf-replace [b cur txt]
  (let [[cur1 cur2] (cursor-sort (:cursor b) cur)
        {lines :lines cursor :cursor} (replace-range (:lines b) [cur1 cur2] txt)]
    (merge b {:lines lines
              :cursor (merge cursor 
                             {:lastcol (:col cursor)
                              :vprow (-> (:row cursor)
                                         (- (:row cur1)) 
                                         (+ (:vprow cur1))
                                         (bound-range 0 (-> b :viewport :h dec)))})})))

(defn buf-insert 
  "Insert at cursor"
  [b txt]
  (buf-replace b (:cursor b) txt))

(defn buf-delete
  "Delete cursor left char"
  [b]
  (let [{row :row col :col vprow :vprow} (:cursor b)] 
    (cond 
      (and (= col 0) (> row 0))
      (buf-replace b (merge (:cursor b) 
                            {:row (dec row) :col (col-count b (dec row)) :vprow (dec vprow)})
                   "")
      (> col 0)
      (buf-replace b (assoc (:cursor b) :col (dec col))
                   "")
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
         {:row (-> b :lines count dec) :col 0 :lastcol 0 :vprow (-> b :viewport :h dec)}))
    
(def re-word-start #"\W(?=\w)|[\s\w](?=[^\s\w])")
(def re-word-start-line-start #"^(?=\S)|\W(?=\w)|[\s\w](?=[^\s\w])")
(def re-word-start-back #"\W(?=\w)|[\s\w](?=[^\s\w])|^(?=\S)")

(def re-WORD-start #"\s(?=\S)")
(def re-WORD-start-line-start #"^(?=\S)|\s(?=\S)")
(def re-WORD-start-back #"\s(?=\S)|^(?=\S)")

(def re-word-end #"(?=\S$)|(?=\w\W)|(?=[^\s\w][\s\w])")
(def re-WORD-end #"(?=\S$)|(?=\S\s)")

(defn line-next-re
  "Move to next charactor match by re." 
  [line col re]
  (let [subline (subs line col)
        m (re-matcher re subline)]
    (if (.find m)
      [true (-> m .end (+ col))]
      [false col])))

(defn line-back-re
  "Move to backious charactor match by re. col=-1 means match whole line" 
  [line col re]
  (let [subline (if (= -1 col)
                  line
                  (subs line 0 col))
        m (re-matcher re subline)]
    (if (.find m)
      (loop [lastend (.end m)]
        (println lastend)
        (if (.find m)
          (recur (.end m))
          [true lastend]))
      [false col])))

(defn cursor-next-re
  "Match re line by line (re not match multiple lines), don't change cursor if not finded"
  [b re re-line-start]
  (loop [row (-> b :cursor :row)
         col (-> b :cursor :col)
         re-current re]
    (if (< row (-> b :lines count))
      (let [line (-> b :lines (get row))
            [matched newcol] (line-next-re line col re-current)]
        (if (not matched)
          (recur (inc row) 0 re-line-start)
          (-> b 
              (assoc-in [:cursor :row] row)
              (assoc-in [:cursor :vprow] (-> row 
                                             (- (-> b :cursor :row)) 
                                             (+ (-> b :cursor :vprow))
                                             (bound-range 0 (-> b :viewport :h dec))))
              (assoc-in [:cursor :col] newcol)
              (assoc-in [:cursor :lastcol] newcol))))
      b)))

(defn cursor-back-re
  "Match re line by line in reverse"
  [b re]
  (loop [row (-> b :cursor :row)
         col (-> b :cursor :col)]
    (if (>= row 0)
      (let [line (-> b :lines (get row))
            [matched newcol] (line-back-re line col re)]
        (if (not matched)
          (recur (dec row) -1)
          (-> b 
              (assoc-in [:cursor :row] row)
              (assoc-in [:cursor :vprow] (-> (-> b :cursor :vprow)
                                             (- (-> b :cursor :row)) 
                                             (+ row)
                                             (bound-range 0 (-> b :viewport :h dec))))
              (assoc-in [:cursor :col] newcol)
              (assoc-in [:cursor :lastcol] newcol))))
      b)))

(defn cursor-back-word
  "The \"b\" motion."
  [b]
  (-> b 
      (cursor-back-re re-word-start-back)))

(defn cursor-back-WORD
  "The \"B\" motion."
  [b]
  (-> b 
      (cursor-back-re re-WORD-start-back)))


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
      (update-in [:cursor :col] inc)
      (cursor-next-re re-word-end re-word-end)))

(defn cursor-WORD-end
  "The \"E\" motion."
  [b]
  (-> b 
      (update-in [:cursor :col] inc) 
      (cursor-next-re re-WORD-end re-WORD-end)))

(defn cursor-line-first
  "The \"0\" motion"
  [b]
  (if (-> b :cursor :col zero?)
    b
    (-> b
        (assoc-in [:cursor :col] 0) 
        (assoc-in [:cursor :lastcol] 0))))

(defn cursor-line-start
  "The \"^\" motion"
  [b]
  (let [line (-> b :lines (get (-> b :cursor :row)))
        [matched col] (line-next-re line 0 #"\s(?=\S)")]
    (if matched
      (-> b
          (assoc-in [:cursor :col] col) 
          (assoc-in [:cursor :lastcol] col))
      b)))

(defn cursor-line-end
  "The \"$\" motion"
  [b]
  (let [col (-> b :lines (get (-> b :cursor :row)) count dec)]
    (-> b
        (assoc-in [:cursor :col] col)
        (assoc-in [:cursor :lastcol] col))))

(defn cursor-move-char
  "Move one character. Direction 0,1,2,3 -> left,right,up,down"
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
                    (and (= direction 3) (> (-> b :lines count) (inc row)))
                    (let [newvprow 
                          (if (< vprow (-> b :viewport :h dec))
                            (inc vprow)
                            (-> b :viewport :h dec))]
                      {:row (inc row) :col (calc-col b (inc row) col lastcol) :vprow newvprow})

                    :else (:cursor b))))))

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
  "Jump cursor by viewport height, deps to buffer's :viewport, keep cursor's viewport row unchanged."
  [b factor]
  (let [d (round-to-zero (* (:h (:viewport b)) factor))
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
                {:vprow (int (/ (-> b :viewport :h) 2))})))


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
          (do (.write wrtr line)
              (.write wrtr "\n"))))
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
    (buf-replace b (assoc (:cursor b) :col (- col (count subject))) word)))
