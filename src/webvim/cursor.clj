(ns webvim.cursor
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [clojure.java.io :as io])
  (:use clojure.pprint
        (clojure [string :only (join split)])
        webvim.autocompl
        webvim.history
        webvim.global))

(def re-word-start #"(?<=\W)\w|(?<=[\s\w])[^\s\w]")
(def re-word-start-line-start #"^\S|(?<=\W)\w|(?<=[\s\w])[^\s\w]")
(def re-word-start-back #"(?<=\W)\w|(?<=[\s\w])[^\s\w]|^\S")

(def re-WORD-start #"(?<=\s)\S")
(def re-WORD-start-line-start #"(?=^\S)|(?<=\s)\S")
(def re-WORD-start-back #"(?<=\s)\S|(?=^\S)")

(def re-word-end #"(?=\S$)|(?=\w\W)|(?=[^\s\w][\s\w])")
(def re-WORD-end #"(?=\S$)|(?=\S\s)")

(defn cursor-sort [cur1 cur2]
  (let [{r1 :row c1 :col} cur1
        {r2 :row c2 :col} cur2]
        (if (or (< r1 r2) (and (= r1 r2) (<= c1 c2)))
          [cur1 cur2]
          [cur2 cur1])))

(defn cursor-to-point [{row :row col :col}]
  {:row row :col col})

(defn buf-change-cursor-col[b col]
  (-> b
      (assoc-in [:cursor :col] col)
      (assoc-in [:cursor :lastcol] col)))

(defn calc-col [b row col lastcol]
  "set col to lastcol if length is avaliable"
  (let [cnt (col-count b row)]
    (cond 
      (> cnt lastcol) lastcol
      (< cnt 1) 0
      :else (dec cnt))))

(defn line-next-re
  "Move to next charactor match by re." 
  [line col re]
  (let [subline (subs line col)
        m (re-matcher re subline)]
    (if (.find m)
      [(-> m .start (+ col)) (-> m .end (+ col))]
      nil)))

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

(defn cursor-next-re
  "Match re line by line (re not match multiple lines), don't change cursor if nothing is found"
  [b re re-line-start]
  (let [[matched newcursor] (lines-next-re b re re-line-start)]
    (if (nil? matched)
      b
      (assoc b :cursor newcursor))))


(defn cursor-move-start
  "Move to beginning of a buffer"
  [b]
  (assoc b :cursor {:row 0 :col 0 :lastcol 0 :vprow 0}))

(defn cursor-move-end
  "Move to first char of last line"
  [b]
  (assoc b :cursor
         {:row (-> b :lines count dec dec) :col 0 :lastcol 0 :vprow (-> @window :viewport :h dec)}))

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

(defn cursor-line-end
  "The \"$\" motion"
  [b]
  (let [col (-> b :lines (get (-> b :cursor :row)) count dec)
        col1 (if (neg? col) 0 col)]
      (buf-change-cursor-col b col1)))

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
