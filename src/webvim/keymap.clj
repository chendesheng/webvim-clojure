(ns webvim.keymap
  (:require [me.raynes.fs :as fs]
            [ring.adapter.jetty :as jetty]
            [clojure.core.async :as async])
  (:use clojure.pprint
        webvim.buffer
        webvim.history
        webvim.cursor
        webvim.global
        webvim.jumplist
        webvim.ex
        webvim.serve
        webvim.text
        webvim.autocompl))

(defonce motion-keymap (atom {}))
(defonce edit-keymap (atom {}))
(defonce normal-mode-keymap (atom {}))
(defonce visual-mode-keymap (atom {}))
(defonce insert-mode-keymap (atom {}))
(defonce ex-mode-keymap (atom {}))

(defn set-normal-mode[b]
  ;(println "set-normal-mode:")
  (merge b {:ex "" 
            :mode normal-mode 
            :keys nil
            :visual {:type 0 :ranges []}
            :autocompl {:suggestions nil 
                        :suggestions-index 0}}))

(defn set-visual-mode[b]
  (println "set-visual-mode:")
  (let [pos (b :pos)]
    (merge b {:ex "" :mode visual-mode :keys nil 
              :visual {:type 0 :ranges [pos pos]}})))

(defn set-insert-mode[b keycode]
  ;(println "set-insert-mode")
  (-> b 
      (merge {:ex "" :mode insert-mode :message nil :keys nil})
      buf-save-cursor))

(defn set-insert-append[b keycode]
  (let [col (if (= (col-count b (-> b :cursor :row)) 1) ;contains only EOL
              0
              (-> b :cursor :col inc))]
    (-> b
        char-forward
        (set-insert-mode keycode))))

(defn set-insert-line-end[b keycode]
  (-> b
      line-end
      (set-insert-mode keycode)))

(defn set-insert-line-start[b keycode]
  (-> b
      line-start
      (set-insert-mode keycode)))

(defn set-insert-remove-char[b keycode]
  (registers-put (:registers b) (-> b :context :register) (buf-copy-range b (:cursor b) (:cursor b) true))
  (-> b
      (set-insert-mode keycode)
      (text-delete-offset 1)))

(defn set-insert-new-line[b keycode]
  (-> b 
      (set-insert-mode keycode)
      text-insert-line-after
      buf-indent-current-line))

(defn set-insert-new-line-before[b keycode]
  (-> b 
      (set-insert-mode keycode)
      text-insert-line-before
      buf-indent-current-line))

(defn insert-mode-default[b keycode]
  (let [b1 (cond 
             (= "backspace" keycode)
             (text-delete-offset b -1)
             (= "enter" keycode)
             (text-insert b "\n")
             (= "space" keycode)
             (text-insert b " ")
             (= "tab" keycode)
             (text-insert b "\t") 
             (= 1 (count keycode))
             (text-insert b keycode)
             :else
             b)
        b2 (buf-update-highlight-brace-pair b1 (:cursor (dec-col b1)))
        b3 (if (re-test (-> b2 :language :indent-triggers) keycode)
             (-> b2 buf-indent-current-line inc-col)
             b2)]
    (if (empty? (-> b3 :autocompl :suggestions))
      b3
      (let [word (buffer-word-before-cursor b3)
            suggestions (autocompl-suggest @autocompl-words word)]
        (if (= 1 (count suggestions))
          (assoc-in b3 [:autocompl :suggestions] [])
          (assoc b3 :autocompl 
                 (merge (:autocompl b3) 
                        {:suggestions suggestions
                         :suggestions-index 0})))))))

(defn highlight-all-matches[b re]
  b)
  ;(loop [b1 (re-forward-highlight b re)
  ;       hls (:highlights b1)]
  ;  (if (= (b1 :pos) (b :pos))
  ;    (assoc b :highlights (vec hls))
  ;    (let [b2 (re-forward-highlight b1 re)]
  ;      (recur b2 (concat hls (:highlights b2)))))))

(defn- buf-cursor-info[b]
  (let [{nm :name 
         path :filepath
         {r :row c :col} :cursor
         lines :lines} b
        cnt (count lines)
        percent (int (-> r (/ cnt) (* 100)))]
    (assoc b :message (str "\"" (or path nm) "\" line " (inc r) " of " (count lines) " --" percent "%-- col " c))))


(defn handle-search[b]
  (registers-put (:registers b) "/" (b :ex))
  (-> b
      (highlight-all-matches (re-pattern (subs (b :ex) 1)))
      (assoc :ex "")))

(def map-key-inclusive 
  {"h" false
   "l" false
   "w" false 
   "W" false 
   "e" true 
   "E" true 
   "b" false 
   "B" false 
   "f" true
   "F" false
   "t" true
   "T" false
   "/" false
   "$" false})

(defn inclusive? [keycode]
  (let [res (map-key-inclusive keycode)]
    (if (nil? res)
      true
      res)))

(defn- buf-copy-range-lastbuf[b cur inclusive]
;  (println (str "inclusive:" inclusive))
;  (println (str "cursor:" cur))
;  (println (str "cursor2:" (:cursor b)))
  (registers-put (:registers b) (-> b :context :register) (buf-copy-range b cur (:cursor b) inclusive))
  b)

(defn same-pos? 
  "return true if :row and :col are equal"
  [cur1 cur2]
  (and (= (:row cur1) (:row cur2)) (= (:col cur1) (:col cur2))))

(defn delete-motion[b keycode]
  ;(println (str "delete-motion:" keycode))
  ;(println (str "inclusive:" (inclusive? keycode)))
  (let [lastbuf (-> b :context :lastbuf)
        pos (b :pos)]
    (if (or (nil? lastbuf) (= pos (:pos lastbuf))) ;don't do anything if cursor doesn't change
      b
      (let [inclusive (inclusive? keycode)]
        (-> lastbuf
            buf-save-cursor
            (buf-copy-range-lastbuf (:cursor b) inclusive)
            (text-delete (if inclusive (inc pos) pos))
            history-save)))))

(defn change-motion[b keycode]
  ;(println "change-motion:" keycode)
  (let [lastbuf (-> b :context :lastbuf)
        lastcur (:cursor lastbuf)
        pos (b :pos)]
    ;(pprint lastcur)
    ;(pprint (:cursor b))
    (if (or (nil? lastbuf) (= pos (:pos lastbuf)))
      b
      (let [inclusive (inclusive? keycode)]
        (-> lastbuf
            ;(println (str "change-motion:" keycode))
            (buf-copy-range-lastbuf lastcur inclusive)
            (text-delete (if inclusive (inc pos) (dec pos)))
            (serve-keymap (@normal-mode-keymap "i") keycode))))))

(defn yank-motion[b keycode]
  (let [lastbuf (-> b :context :lastbuf)
        lastcur (:cursor lastbuf)]
    (if (or (nil? lastbuf) (same-pos? (:cursor b) lastcur))
      b
      (let [inclusive (inclusive? keycode)]
        (buf-copy-range-lastbuf b lastcur inclusive)
        lastbuf))))

(defn indent-motion[b keycode]
  (let [lastbuf (-> b :context :lastbuf)
        lastcur (:cursor lastbuf)]
    (if (or (nil? lastbuf) (same-pos? (:cursor b) lastcur))
      b
      (-> b 
          buf-save-cursor
          (buf-indent-lines (lastcur :row))
          (history-save)))))

(defn start-register[b keycode]
  (let [m (re-find #"[0-9a-zA-Z/*#%.:+=\-]" keycode)]
    (if (not (nil? m))
      (-> b 
          (assoc-in [:context :register] keycode)
          (serve-keymap (select-keys @normal-mode-keymap ["y" "d" "c" "p" "P"]) keycode)))))


(defn visual-mode-select[b keycode]
  (let [m (re-find #"[ocd]" keycode)] ;don't change cursor position if not motion
    (if (nil? m)
      (assoc-in b [:visual :ranges 1]
                (b :pos)) b)))

(defn autocompl-start[b]
  (let [word (buffer-word-before-cursor b)
        suggestions (autocompl-suggest @autocompl-words word)]
    (println "autocompl:")
    (println word)
    (assoc b :autocompl 
           {:suggestions suggestions 
            :suggestions-index 0})))

(defn autocompl-move[b f]
  (let [b1 (if (empty? (-> b :autocompl :suggestions))
             (autocompl-start b)
             b)
        i (f (-> b1 :autocompl :suggestions-index))
        cnt (-> b1 :autocompl :suggestions count)]
    (if (zero? cnt)
      b1
      (let [n (mod (+ i cnt) cnt)
            w (-> b1 :autocompl :suggestions (get n))
            s (-> b1 :autocompl :suggestions (get 0))
            ;delete back then insert word
            ks (apply conj (vec (repeat (count s) "backspace")) (map str (vec w)))]
        (-> b1 
            (assoc-in [:autocompl :suggestions-index] n)
            (update-in [:macro :recording-keys] 
                      #(apply conj % ks)) 
            (buffer-replace-suggestion w))))))

(defn keycode-to-char[keycode]
  (cond 
    (= 1 (count keycode))
    keycode
    (= "tab" keycode)
    "\t"
    (= "space" keycode)
    " "
    :else ""))

(defn move-to-next-char[b keycode]
  (let [ch (keycode-to-char keycode)]
    (re-forward b (-> ch quote-pattern re-pattern))))

(defn move-to-back-char[b keycode]
  (let [ch (keycode-to-char keycode)]
    (re-backward b (-> ch quote-pattern re-pattern))))

(defn move-before-next-char[b keycode]
  (-> b (move-to-next-char keycode)
      char-backward))

(defn move-after-back-char[b keycode]
  (-> b (move-to-back-char keycode)
      char-backward))

(defn put-from-register[b keycode]
  (let [txt (registers-get (:registers b) keycode)]
    (if (string? txt)
      (-> b
          buf-save-cursor
          (buf-insert txt)
          dec-col
          history-save)
      b)))

(defn put-from-register-append[b keycode]
  (let [txt (registers-get (:registers b) keycode)
        col (-> b :cursor :col inc)
        col1 (if (= col (col-count b (-> b :cursor :row)))
               (dec col)
               col)]
    (if (string? txt)
      (-> b
          buf-save-cursor
          (assoc-in [:cursor :col] col1)
          (buf-insert txt)
          dec-col
          history-save)
      b)))

(defn delete-line[b]
  (let [{row :row col :col} (b :cursor)]
    (registers-put (:registers b) (-> b :context :register) (buf-copy-range b {:row row :col 0} {:row row :col (col-count b row)} false))
    (-> b
        (update-in [:context] dissoc :lastbuf) ;remove :lastbuf prevent delete-motion take over.
        buf-save-cursor
        (text-delete-range (current-line b))
        line-start
        history-save)))

(defn delete-to-line-end[b]
  (let [{row :row col :col} (b :cursor)]
    (registers-put (:registers b) (-> b :context :register) (buf-copy-range b {:row row :col col} {:row row :col (-> b (col-count row) dec)} false))
    (-> b
      (update-in [:context] dissoc :lastbuf) ;remove :lastbuf prevent delete-motion take over.
      buf-save-cursor
      (buf-delete-range2 {:row row :col col} {:row row :col (-> b (col-count row) dec)})
      history-save)))

(defn delete-range[b]
  (-> b
      (assoc :last-cursor (-> b :context :lastbuf :cursor))
      (text-delete-inclusive (b :pos) (-> b :context :lastbuf :pos))
      history-save))

(defn change-range[b]
  (-> b 
      (assoc :last-cursor (-> b :context :lastbuf :cursor))
      buf-delete-range
      (set-insert-mode "c")
      (serve-keymap (@normal-mode-keymap "i") "c")))

(defn move-next-same-word[b]
  (let [[start end] (current-word b)
        word (text-subs (b :str) start end)
        _ (println (str word))
        re (re-pattern (str "\\b" (quote-pattern word) "\\b"))]
    (registers-put (:registers b) "/" (str "/" re))
    (-> b 
        (re-forward-highlight re)
        (highlight-all-matches re))))

(defn move-back-same-word[b]
  (let [[start end] (current-word b)
        word (text-subs (b :str) start end)
        re (re-pattern (str "\\b" (quote-pattern word) "\\b"))]
    (registers-put (:registers b) "/" (str "?" re))
    (-> b 
        (re-backward-highlight re)
        (highlight-all-matches re))))

(defn buf-close-chan-in[b]
  (async/close! (:chan-in b))
  b)

(defn replay-keys [b keycodes keymap]
  (let [in (async/chan) ;use local :chan-in :chan-out only for replay keys
        out (async/chan)
        registers (atom @(:registers b))
        b1 (-> b
               (assoc :registers registers) ;Use different reigsters when replay keys, avoid changing to the global registers.
               (assoc :chan-in in)
               (assoc :chan-out out))]
    (println "start replay:")
    (pprint keycodes)
    (key-server b1 keymap)

    ;loop through keycodes, feed :chan-in read :chan-out, return last :chan-out result
    (loop [kc (first keycodes)
           coll (subvec keycodes 1)]
      (let[_ (async/>!! in kc) 
           b2 (async/<!! out)]
        (pprint coll)
        (if (empty? coll)
          (-> b2
              buf-close-chan-in
              ;restore back
              (assoc :registers (:registers b))
              (assoc :chan-in (:chan-in b))
              (assoc :chan-out (:chan-out b)))
          (recur (first coll) (subvec coll 1)))))))

(defn save-x-to-vx[b]
  (assoc b :vx (b :x)))

(defn save-x-if-not-jk
  "save to :vx unless it is up down motion"
  [b lastbuf keycode]
  (if-not (or (= (:pos lastbuf) (:pos b)) 
              (contains? #{"j" "k"} keycode))
    (save-x-to-vx b) b))

(defn normal-mode-fix-pos
    "prevent cursor on top of EOL in normal mode"
    [b]
    (let [ch (text-char-at (b :str) (b :pos))]
      (if (= (or ch \newline) \newline)
        (char-backward b) b)))

(defn normal-mode-after[b keycode]
  (let [{col :col row :row} (:cursor b)
        lastbuf (-> b :context :lastbuf)]
    (if-not (nil? (motions-push-jumps keycode))
      (jump-push lastbuf))
    ;(println "normal-mode-after, recording-keys" (-> b :macro :recording-keys))
    ;if nothing changed there is no need to overwrite "." register
    ;so keys like i<esc> won't affect, this also exclude all motions.
    (if-not (or (= (:lines lastbuf) (:lines b))
                 ;These commands should not get repeat
                 (contains? #{".", "u", "c+r", "p", "P", ":"} keycode))
      (registers-put (:registers b) "." (-> b :macro :recording-keys)))
    (-> b 
        normal-mode-fix-pos
        (save-x-if-not-jk lastbuf keycode)
        ;alwasy clear :recording-keys
        (assoc-in [:macro :recording-keys] [])
        (buf-update-highlight-brace-pair (:cursor b)))))

(defn dot-repeat[b]
  (let [keycodes (registers-get (:registers b) ".")]
    (if (empty? keycodes)
      b
      (let [{in :chan-in out :chan-out} b] 
        ;remove "." from normal-mode-keymap prevent recursive, probably useless
        (replay-keys b keycodes (dissoc @normal-mode-keymap "."))))))

(defn cut-char[b]
  (if (= (col-count b (-> b :cursor :row)) 1)
    b
    (let [cursor (:cursor b)]
      (registers-put (:registers b) (-> b :context :register) (buf-copy-range b cursor cursor true))
      (-> b 
          buf-save-cursor
          (buf-replace cursor "" true)
          history-save))))

(defn yank-visual[b]
  (let [[p1 p2] (-> b :visual :ranges)]
    (registers-put (:registers b) (-> b :context :register) (buf-copy-range b p1 p2 true))
    (let [[cur1 _] (apply cursor-sort (-> b :visual :ranges))]
      (-> b 
          (assoc :cursor {:row (:row cur1)
                          :col (:col cur1)
                          :lastcol (:col cur1)})))))

(defn inside? [lines {r :row c :col}]
  (and (< r (count lines)) (< c (count (lines r)))))

(defn move-to-jumplist
  [b fndir]
  (loop [pos (fndir b)]
    (if (nil? pos)
      b ;newest or oldest
      (let [newb (@buffer-list (pos :id))]
        (if (nil? newb)
          ;buffer has been deleted, ignore
          (recur (fndir b)) 
          ;pos is avaliable
          (if (inside? (newb :lines) (pos :cursor))
            (let [newid (newb :id)
                  newcur (pos :cursor)]
              (if (= newid @active-buffer-id) 
                ;update pos inside current buffer
                (assoc b :cursor newcur)
                (let []
                  (change-active-buffer newid)
                  (swap! buffer-list assoc-in [newid :cursor] newcur)
                  b)))
            ;buffer has been modifed and cursor is no longer inside, ignore
            (recur (fndir b))))))))

(defn- nop[b] b)

(defn init-keymap-tree
  []
  (reset! ex-mode-keymap
          {:continue #(not (or (= "esc" %2) (= "enter" %2) (empty? (:ex %1))))
           :leave (fn[b keycode]
                    (if (and (= "esc" keycode) (= \/ (-> b :ex first)))
                      (-> b :context :lastbuf (assoc :ex ""))
                      (assoc b :ex "")))
           :else ex-mode-default})

  (reset! motion-keymap
          {;"h" #(cursor-move-char % 0)
           ;"l" #(cursor-move-char % 1)
           "h" char-backward
           "l" char-forward
           "k" #(lines-backward % 1)
           "j" #(lines-forward % 1)
           "g" {"g" text-start}
           "G" cursor-move-end
           "w" word-forward
           "W" WORD-forward
           "b" word-backward
           "B" WORD-backward
           "e" word-end-forward
           "E" WORD-end-forward
           "0" line-first
           "^" line-start
           "$" line-end
           "f" {"esc" nop
                :else move-to-next-char }
           "F" {"esc" nop
                :else move-to-back-char }
           "t" {"esc" nop 
                :else move-before-next-char }
           "T" {"esc" nop 
                :else move-after-back-char }
           "/" (merge @ex-mode-keymap 
                      {"enter" handle-search
                       :enter set-ex-search-mode
                       :else ex-mode-default})
           "?" (merge @ex-mode-keymap
                      {"enter" handle-search
                       :enter set-ex-search-mode
                       :else ex-mode-default})
           "*" move-next-same-word
           "#" move-back-same-word
           "n" #(let[s (or (registers-get (:registers %) "/") "/")
                     dir (first s)
                     re (re-pattern (subs s 1))
                     fnsearch (if (= \/ dir) re-forward-highlight re-backward-highlight)]
                  (-> % 
                      (dissoc :highlights)
                      (fnsearch re)
                      (highlight-all-matches re)))
           "N" #(let[s (or (registers-get (:registers %) "/") "?")
                     dir (or (first s) "")
                     re (re-pattern (subs s 1))
                     fnsearch (if (= \/ dir) re-backward-highlight re-forward-highlight)]
                  (-> % 
                      (dissoc :highlights)
                      (fnsearch re)
                      (highlight-all-matches re)))
           "}" paragraph-forward
           "{" paragraph-backward
           "%" (fn[b]
                 (let [b1 (if (not (contains? all-braces (char-under-cursor b)))
                            (cursor-next-re b #"[\(\[\{\)\]\}]" #"[\(\[\{\)\]\}]")
                            b)]
                   (let [cur (cursor-match-brace b1)]
                     (if (nil? cur) 
                       b1
                       (let [newcur (assoc cur :lastcol (:col cur))]
                         (assoc b1 :cursor newcur))))))
           "c+u" #(cursor-move-viewport %1 -0.5) 
           "c+d" #(cursor-move-viewport %1 0.5)})

  (reset! visual-mode-keymap @motion-keymap)
  (swap! visual-mode-keymap 
         merge
         {"z" {"z" cursor-center-viewport}
          "y" yank-visual})

  (reset! insert-mode-keymap 
          {;"c+o" @normal-mode-keymap 
           "c+n" #(autocompl-move % inc)
           "c+p" #(autocompl-move % dec)
           "c+r" {"esc" #(assoc % :keys [])
                  :else put-from-register }
           :else insert-mode-default 
           :enter set-insert-mode
           :continue #(not (= "esc" %2))
           :leave (fn[b keycode]
                    (-> b
                        char-backward
                        save-x-to-vx
                        set-normal-mode
                        history-save))})

  (reset! normal-mode-keymap @motion-keymap)
  (swap! normal-mode-keymap 
         merge 
         {"w" word-forward
          "W" WORD-forward

          "i" @insert-mode-keymap
          "a" (merge
                @insert-mode-keymap
                {:enter set-insert-append})
          "A" (merge
                @insert-mode-keymap
                {:enter set-insert-line-end})
          "I" (merge
                @insert-mode-keymap
                {:enter set-insert-line-start})
          "s" (merge
                @insert-mode-keymap
                {:enter set-insert-remove-char})
          "O" (merge
                @insert-mode-keymap
                {:enter set-insert-new-line-before})
          "o" (merge
                @insert-mode-keymap
                {:enter set-insert-new-line})
          "." dot-repeat
          "=" (merge 
                @motion-keymap
                {"=" #(-> % 
                         (update-in [:context] dissoc :lastbuf)
                         buf-save-cursor
                         buf-indent-current-line
                         history-save)
                 :before save-lastbuf
                 :after indent-motion})
          ":" (merge
                @ex-mode-keymap
                {"enter" execute
                 :enter (fn[b keycode] (set-ex-mode b))
                 :leave (fn[b keycode] (set-normal-mode b))})
          "r" {"esc" nop
               "enter" (fn [b]
                         (-> b
                             buf-save-cursor
                             (buf-replace-char "\n")
                             (assoc-in [:cursor :col] 0)
                             (update-in [:cursor :row] inc)
                             buf-indent-current-line
                             history-save))
               :else (fn[b keycode]
                       (let [ch (cond
                                  (= keycode "space")
                                  " "
                                  :else keycode)]
                         (if (= (count ch) 1)
                           (-> b 
                               buf-save-cursor
                               (buf-replace-char ch)
                               history-save)
                               b)))}
          "u" history-undo
          "c+r" history-redo
          "c+o" #(move-to-jumplist % jump-prev)
          "c+i" #(move-to-jumplist % jump-next)
          "c+g" buf-cursor-info
          "esc" set-normal-mode
          "c+l" #(dissoc % :highlights) 
          "v" (merge
                @visual-mode-keymap
                {:enter (fn[b keycode] 
                          (-> b
                              set-visual-mode
                              (assoc-in [:context :lastbuf] b)))
                 :leave (fn[b keycode] (set-normal-mode b))
                 :continue #(not (or (= "d" %2) (= "c" %2) (= "esc" %2) (= "v" %2) (= "y" %2)))
                 :after (fn[b keycode]
                          (-> b
                              (visual-mode-select keycode)
                              (save-x-if-not-jk (b :lastbuf) keycode)))
                 "d" delete-range
                 "c" change-range
                 "o" (fn[b]
                       (let [{r :row c :col} (b :cursor)
                             [pt1 pt2] (-> b :visual :ranges)
                             {r1 :row c1 :col} pt1
                             newb (-> b 
                                      (assoc-in [:cursor :col] c1)
                                      (assoc-in [:cursor :lastcol] c1)
                                      (assoc-in [:cursor :row] r1)
                                      (update-in [:visual :ranges] 
                                                 (fn[[a b]][b a])))]
                         (assoc-in newb [:context :lastbuf] newb)))})
          "z" {"z" cursor-center-viewport }
          "d" (merge 
                @motion-keymap
                {:before save-lastbuf 
                 :after delete-motion
                 ;"j" delete-line
                 ;"k" delete-line
                 "d" delete-line})
          "x" cut-char
          "p" #(put-from-register-append % (-> % :context :register))
          "P" #(put-from-register % (-> % :context :register))
          "D" delete-to-line-end
          "J" (fn[b]
                (-> b 
                    buf-save-cursor
                    buf-join-line
                    history-save))
          "c" (merge
                @motion-keymap 
                {:before save-lastbuf
                 :after change-motion})
          "y" (merge
                @motion-keymap
                {:before save-lastbuf
                 :after yank-motion})
          "\"" {:else start-register}
          :before (fn [b keycode]
                    (-> b
                        (assoc-in [:context :lastbuf] b)
                        (assoc-in [:context :register] "\"")))
          :after normal-mode-after})
(reset! root-keymap @normal-mode-keymap))
