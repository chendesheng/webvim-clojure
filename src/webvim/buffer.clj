(ns webvim.buffer
  (:require [me.raynes.fs :as fs]
            [clojure.java.io :as io])
  (:use clojure.pprint
        (clojure [string :only (join split)])))

(defn split-lines-all [txt]
  (split txt #"\r?\n" -1))

(defn open-file[f]
  "Create buffer from a file"
  (let [txt (slurp f)]
  {:lines (split-lines-all txt)
   ;row, col, lastcol, viewport row (row from top of current viewport)
   :cursor {:row 0 :col 0 :lastcol 0 :vprow 0}
   :sel-range []
   :mode 0
   :name f
   ;ongoing ex command
   :ex ""
   ;ongoing command keys, display beside "-- MODE --" prompt
   :keys []
   ;undo/redo history
   :history {:items []
             :version 0}
   ;viewport size
   :viewport {:w 0 :h 0}}))
   ;TODO: put all server only states (like :viewport) into one map and don't send to client

(defonce active-buffer (atom {}))

;only for testing on repl
(reset! active-buffer (open-file "testfile.clj"))

(defn history-changed?[b]
  (if (zero? (-> b :history :version))
    true
    (let [item ((-> b :history :items) (-> b :hisotry :version dec))]
      (not (= (:lines b) (:lines item))))))

(defn history-save
  "push state into history, only :lines and :cursor is needed"
  [b]
  (if (history-changed? b)
    (let [version (-> b :history :version)
          items (subvec (-> b :history :items) 0 version)]
      (assoc b :history {:items 
                         (conj items
                               {:lines (:lines b) 
                                :cursor (:cursor b)})
                         :version 
                         (inc version)}))
    b))

(defn history-undo[b]
  (if (pos? (-> b :history :version))
    (let [newversion (-> b :history :version dec)]
      (merge b 
             (->> b :history :items newversion)
             {:history 
              {:items (-> b :history :items) 
               :version newversion}}))
    b))

(defn history-redo[b]
  (if (< (-> b :history :version) (-> b :history :items count))
    (let [newversion (-> b :history :version inc)]
      (merge b 
             (->> b :history :items newversion)
             {:history 
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
                  :col newcol
                  :lastcol newcol}}))
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
                :col newcol 
                :lastcol newcol}})))

(defn merge-lines-cursor
  "Merge second buffer's :lines and :cursor to first buffer"
  [b1 b2]
  (merge b1 {:lines (:lines b2) 
            :cursor (merge 
                      (:cursor b1) 
                      (:cursor b2))}))

(defn buf-insert [b txt]
  "Insert at cursor"
  (let [res (replace-range 
              (:lines b) 
              [(:cursor b) (:cursor b)]
              txt)]
    (merge-lines-cursor b res)))

(defn buf-delete
  "Delete cursor left char"
  [b]
  (println "buf-delete")
  (let [{row :row col :col} (:cursor b)] 
    (cond 
      (and (= col 0) (> row 0))
      (merge-lines-cursor b (replace-range 
                              (:lines b) 
                              [{:row (dec row) :col (col-count b (dec row))} (:cursor b)] ""))
      (> col 0)
      (merge-lines-cursor b (replace-range
                              (:lines b)
                              [{:row row :col (dec col)} (:cursor b)] ""))
      :else b)))

(defn calc-col [b row col lastcol]
  "set col to lastcol if length is avaliable"
  (let [cnt (col-count b row)]
    (cond 
      (> cnt lastcol) lastcol
      (< cnt 1) 0
      :else (dec cnt))))

;(calc-col test-buf 4 30 30)

(defn cursor-move-start
  "Move to beginning of a buffer"
  [b]
  (assoc b :cursor {:row 0 :col 0 :lastcol 0 :vprow 0}))

(defn cursor-move-end
  "Move to first char of last line"
  [b]
  (pprint (dissoc b :lines))
  (assoc b :cursor
         {:row (-> b :lines count dec) :col 0 :lastcol 0 :vprow (-> b :viewport :h dec)}))


(defn cursor-move-char
  "Move one character. Direction 0,1,2,3 -> left,right,up,down"
  [b direction]
  (let [{row :row col :col lastcol :lastcol vprow :vprow } (:cursor b)]
    (pprint (:cursor b))
    (println "col-count:" (col-count b row))
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
                      (println c)
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

;(defn count-lines [b]
;  (count (:lines b)))

;(count-lines test-buf)
;(= (join "\n" (:lines test-buf)) (:text test-buf))

;(defn insert-single-line [line s txt]
;  (let 
;    [prefix (subs line 0 s)
;     suffix (subs line s)
;     lines (split-lines-all txt)]
;    (update 
;      (update 
;        lines 0 #(str prefix %))
;      (dec (count lines))
;      #(str % suffix))))
;
;(insert-single-line "hello" 2 "yes\nyes")

;(replace-range ["hello" "ok" "yes"] [[0 0] [1 1]] "insert")
;(replace-range ["hello" "ok" "yes"] [[1 1] [1 1]] "in\nsert")
;(replace-range ["hello" "ok" "yes"] [[2 0] [2 3]] "in\nsert")
;(def test-buf (cursor-move-char
; (cursor-move-char test-buf 2) 2))
;
;(def test-buf (assoc test-buf :cursor [5 30 30]))
;(:cursor (cursor-move-char test-buf 2))
;(def test-buf (buf-insert test-buf "\n"))
;(count (:lines test-buf))
;(def test-buf (replace-range (:lines test-buf) (:cursor test-buf) "a"))

