(ns webvim.keymap
  (:require [ring.adapter.jetty :as jetty]
            [clojure.core.async :as async]
            [ring.util.response :as response])
  (:use clojure.pprint
        webvim.buffer
        webvim.global
        webvim.jumplist
        webvim.autocompl))

(defonce motion-keymap (atom {}))
(defonce edit-keymap (atom {}))
(defonce normal-mode-keymap (atom {}))
(defonce visual-mode-keymap (atom {}))
(defonce insert-mode-keymap (atom {}))
(defonce ex-mode-keymap (atom {}))

;enter point of key sequence parser
(defonce root-keymap (atom {}))

(defonce normal-mode 0)
(defonce insert-mode 1)
(defonce visual-mode 2)
(defonce ex-mode 3)

(declare serve-keymap)
(declare map-fn-else)

(defn call-if-fn [b f & args]
  (if (fn? f)
    (apply f b args)
    b))

(defn- record-keys[b keycode]
  (if (nil? (#{"c+n" "c+p" "c+a" "c+x"} keycode)) ;Don't record keycode for these commands
    (update-in b [:macro :recording-keys] conj keycode)
    b))

;two types: key (leaf), keymap (internal node)
;when visit a keymap call :enter :leave 
; keymap is repetitive if :continue return true
;when visit a key call :before :after
(defn serve-keys
  "Serve a sequence of keys until end of keymap. Recusivly walk through keymap tree (works like sytax parser)"
  [b keymap keycode]
  (let [f (keymap keycode)]
    (if (or (fn? f) (map? f) (fn? (:else keymap)))
      (-> b
          (record-keys keycode)
          (call-if-fn (:before keymap) keycode)
          (map-fn-else keymap keycode)
          (call-if-fn (:after keymap) keycode))
      b)))

(defn map-fn-else[b keymap keycode]
  (let [f (keymap keycode)]
    (cond
      (map? f)
      (serve-keymap b f keycode)
      (fn? f)
      (f b)
      (nil? f)
      (call-if-fn b (:else keymap) keycode))))

(defn loop-serve-keys[b keymap]
  (let [keycode (async/<!! (:chan-in b))
        b1 (serve-keys b keymap keycode)]
    (if (and (fn? (:continue keymap))
             ((:continue keymap) b1 keycode))
      (let[]
        (async/>!! (:chan-out b1) b1)
        (recur b1 keymap))
      [b1 keycode])))

(defn send-out
  "write to out channel then return first argument"
  [obj out]
  (let[]
    (async/>!! out obj)
    obj))

(defn serve-keymap[b keymap keycode]
  (let [b1 (-> b
               (buffer-append-keys keycode)
               (call-if-fn (:enter keymap) keycode)
               (send-out (:chan-out b)))
        [b2 leave-keycode] (loop-serve-keys b1 keymap)]
    (call-if-fn b2 (:leave keymap) leave-keycode)))


(defn key-server-inner[b keymap]
  (let [{in :chan-in out :chan-out} b]
    (try
      (let [keycode (async/<!! in)]
        (if (nil? keycode)
          (async/close! out)
          (-> b
              (serve-keys keymap keycode)
              (buffer-reset-keys)
              (send-out out))))
      (catch Exception e
        (let [err (str "caught exception: " e)]
          (println err)
          (.printStackTrace e)
          (send-out (merge b {:ex "" :mode 0 :message err}) 
                    out))))))

(defn key-server
  "Start a dedicate thread handle input keys. Close :chan-in will stop this thread."
  [b keymap]
  (async/thread 
    (loop[b1 b]
      (if (not (nil? b1))
        (recur (key-server-inner b1 keymap)))))
  b)

(defn set-normal-mode[b]
  (println "set-normal-mode:")
  (merge b {:ex "" 
            :mode normal-mode 
            :keys nil 
            :visual {:type 0 :ranges []}
            :autocompl {:suggestions nil 
                        :suggestions-index 0}}))

(defn set-visual-mode[b]
  (println "set-visual-mode:")
  (let [cur (-> b :cursor cursor-to-point)]
    (merge b {:ex "" :mode visual-mode :keys nil 
              :visual {:type 0 :ranges [cur (cursor-inc-col cur)]}})))

(defn set-insert-mode[b keycode]
  (println "set-insert-mode")
  (-> b 
      (merge {:ex "" :mode insert-mode :message nil :keys nil})
      buf-save-cursor))

(defn set-insert-append[b keycode]
  (let [col (if (= (col-count b (-> b :cursor :row)) 1) ;contains only EOL
              0
              (-> b :cursor :col inc))]
    (-> b
        (set-insert-mode keycode)
        (assoc-in [:cursor :col] col)
        (assoc-in [:cursor :lastcol] col))))

(defn set-insert-line-end[b keycode]
  (let [col (dec (col-count b (-> b :cursor :row)))]
    (-> b
        cursor-line-end
        (set-insert-mode keycode))))

(defn set-insert-line-start[b keycode]
  (-> b
      cursor-line-start
      (set-insert-mode keycode)))

(defn set-insert-remove-char[b keycode]
  (swap! (:registers b) assoc "\"" (buf-copy-range b (:cursor b) (:cursor b) true))
  (-> b
      (set-insert-mode keycode)
      (buf-replace (:cursor b) "" true)))

(defn set-insert-new-line[b keycode]
  (-> b 
      (set-insert-mode keycode)
      buf-insert-line-after
      buf-indent-new-line))

;(set-insert-new-line {:lines ["       aa" "ccc"] :cursor {:row 0 :col 0 :lastcol 0 :vprow 0}} "a")

(defn set-ex-mode[b]
  (merge b {:mode ex-mode :ex ":" :message nil :keys nil}))

(defn set-ex-search-mode[b keycode]
  (println "set-ex-search-mode")
  (-> b 
      (merge {:ex keycode :message nil :keys nil})
      (assoc-in [:context :lastbuf] b)))

(defn insert-mode-default[b keycode]
  (let [b1 (cond 
             (= "backspace" keycode)
             (buf-delete b)
             (= "enter" keycode)
             (buf-insert b "\n")
             (= "space" keycode)
             (buf-insert b " ")
             (= "tab" keycode)
             (buf-insert b "\t") 
             (= 1 (count keycode))
             (buf-insert b keycode)
             :else
             b)
        b2 (buf-update-highlight-brace-pair b1 (:cursor (dec-col b1)))]
    (if (empty? (-> b2 :autocompl :suggestions))
      b2
      (let [word (buffer-word-before-cursor b2)
            suggestions (autocompl-suggest @autocompl-words word)]
        (if (= 1 (count suggestions))
          (assoc-in b2 [:autocompl :suggestions] [])
          (assoc b2 :autocompl 
                 (merge (:autocompl b2) 
                        {:suggestions suggestions
                         :suggestions-index 0})))))))

(defn ex-mode-default[b keycode]
  (let [ex (:ex b)
        newex (cond 
                (= keycode "space")
                (str ex " ")
                (= keycode "backspace")
                (subs ex 0 (-> ex count dec))
                (= 1 (count keycode))
                (str ex keycode)
                :else ex)]
    (cond 
      (= \/ (first newex))
      (let [lb (-> b :context :lastbuf)
            newb (-> lb
                     (assoc :ex newex)
                     (assoc-in [:context :lastbuf] lb))] ;keep lastbuf avaiable on stack
        (try (cursor-next-str newb (subs newex 1))
             (catch Exception e newb)))
      (= \? (first newex))
      (let [lb (-> b :context :lastbuf)
            newb (-> lb
                     (assoc :ex newex)
                     (assoc-in [:context :lastbuf] lb))]
        (try (cursor-back-str newb (subs newex 1))
             (catch Exception e newb)))
      :else
      (assoc b :ex newex))))


(defn move-to-line[b row]
  (-> b 
      (assoc-in [:cursor :row] row)
      (assoc-in [:cursor :vprow] (-> @window :viewport :h (/ 2) int))
      (cursor-line-start)))

(defn highlight-all-matches[b re]
  (loop [b1 (cursor-next-str b re)
         hls (:highlights b1)]
    (if (and (= (-> b1 :cursor :row) (-> b :cursor :row))
             (= (-> b1 :cursor :col) (-> b :cursor :col)))
      (assoc b :highlights (vec hls))
      (let [b2 (cursor-next-str b1 re)]
        (recur b2 (concat hls (:highlights b2)))))))

(defn- buf-info[b]
  (assoc b :message (str "\"" (:name b) "\" " (count (:lines b)) "L")))

(def ex-commands
  {"write" (fn[b _ _]
             (write-buffer b))
   "nohlsearch" (fn[b _ _]
                  (dissoc b :highlights))
   "edit" (fn[b excmd file]
            (println "file:" file)
            (if (or (empty? file) (= file (:name b)))
              b ;TODO maybe we should refresh something when reopen same file?
              (let [newid (-> file
                             open-file
                             buf-info
                             buffer-list-save
                             ;start a new thread handle this file
                             (key-server @root-keymap)
                             (get :id))]
                (reset! active-buffer-id newid)
                (jump-push b)
                b)))
   "bnext" (fn[b execmd args]
             (let [id (-> b :id inc)
                   newid (if (> id @gen-buf-id) 1 id)]
               (reset! active-buffer-id newid)
               (jump-push b)
               b))
   "bprev" (fn[b execmd args]
             (let [id (-> b :id dec)
                   newid (if (< id 1) @gen-buf-id id)]
               (reset! active-buffer-id newid)
               (jump-push b)
               b))
   #"^(\d+)$" (fn[b row _]
                (println "row:" row)
                (jump-push b)
                (let [row (bound-range (dec (Integer. row)) 0 (-> b :lines count dec))]
                  (move-to-line b row)))})

(defn handle-search[b]
  (swap! (:registers b) assoc "/" (b :ex))
  (highlight-all-matches b (subs (b :ex) 1)))

(defn execute [b]
  (let [[_ excmd args] (re-find #"\s*:([^\s]+)\s*(.*)\s*" (:ex b))]
    (if (nil? excmd)
      b
      (let [handlers (filter fn?
                             (map (fn[[cmd handler]]
                                    ;(println cmd)
                                    (if (string? cmd)
                                      (if (zero? (.indexOf cmd excmd)) handler nil)
                                      (let [m (re-find cmd excmd)]
                                        (if (not (nil? m)) handler nil)))) ex-commands))]
        (if (= 1 (count handlers))
          ((first handlers) b excmd args)
          (assoc b :message "unknown command"))))))

(defn save-lastbuf[b keycode]
  (-> b (assoc-in [:context :lastbuf] b)))

(defonce map-key-inclusive 
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
   "$" true})

(defn inclusive? [keycode]
  (let [res (map-key-inclusive keycode)]
    (if (nil? res)
      true
      res)))

(defn- buf-copy-range-lastbuf[b cur inclusive]
  (println (str "inclusive:" inclusive))
  (println (str "cursor:" cur))
  (println (str "cursor2:" (:cursor b)))
  (swap! (:registers b) assoc "\"" (buf-copy-range b cur (:cursor b) inclusive))
  b)

(defn same-pos? 
  "return true if :row and :col are equal"
  [cur1 cur2]
  (and (= (:row cur1) (:row cur2)) (= (:col cur1) (:col cur2))))

(defn delete-motion[b keycode]
  (println (str "delete-motion:" keycode))
  (println (str "inclusive:" (inclusive? keycode)))
  (let [lastbuf (-> b :context :lastbuf)]
    (if (or (nil? lastbuf) (same-pos? (:cursor b) (:cursor lastbuf))) ;don't do anything if cursor doesn't change
      b
      (-> b :context :lastbuf 
          buf-save-cursor
          (buf-copy-range-lastbuf (:cursor b) (inclusive? keycode))
          (buf-replace
            (:cursor b) "" (inclusive? keycode))
          history-save))))

(defn change-motion[b keycode]
  (println "change-motion:" keycode)
  (let [lastbuf (-> b :context :lastbuf)
        lastcur (lastbuf :cursor)]
    (pprint lastcur)
    (pprint (:cursor b))
    (if (or (nil? lastbuf) (same-pos? (:cursor b) lastcur))
      b
      (let [inclusive (inclusive? keycode)]
        (-> b 
            ;(println (str "change-motion:" keycode))
            (buf-copy-range-lastbuf lastcur inclusive)
            (buf-replace lastcur "" inclusive)
            (serve-keymap (@normal-mode-keymap "i") keycode))))))

(defn visual-mode-select[b keycode]
  (println "visual-mode-select:")
  (assoc-in b [:visual :ranges 1]
            (-> b 
                :cursor 
                cursor-inc-col 
                cursor-to-point)))

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

(defn move-to-next-char[b keycode]
  (let [ch (cond 
             (= 1 (count keycode))
             keycode
             (= "tab" keycode)
             "\t"
             (= "space" keycode)
             " ")]
    (cursor-next-char b ch)))

(defn move-to-back-char[b keycode]
  (let [ch (cond 
             (= 1 (count keycode))
             keycode
             (= "tab" keycode)
             "\t"
             (= "space" keycode)
             " ")]
    (cursor-back-char b ch)))

(defn move-before-next-char[b keycode]
  (let [b1 (move-to-next-char b keycode)]
    (if (same-pos? (:cursor b1) (-> b1 :context :lastbuf :cursor))
      b1
      (dec-col b1))))

(defn move-after-back-char[b keycode]
  (let [b1 (move-to-back-char b keycode)]
    (if (same-pos? (:cursor b1) (-> b1 :context :lastbuf :cursor))
      b1
      (inc-col b1))))


(defn put-from-register[b keycode]
  (let [txt (@(:registers b) keycode)]
    (if (string? txt)
      (-> b
          buf-save-cursor
          (buf-insert txt)
          dec-col
          history-save)
      b)))

(defn put-from-register-append[b keycode]
  (let [txt (@(:registers b) keycode)
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
  (-> b
    (update-in [:context] dissoc :lastbuf) ;remove :lastbuf prevent delete-motion take over.
    buf-save-cursor
    buf-delete-line
    history-save))

(defn move-next-same-word[b]
  (let [[word start] (word-under-cursor (:lines b) (:cursor b))
        re (str "\\b" word "\\b")]
    (swap! (:registers b) assoc "/" (str "/" re))
    (-> b 
        (assoc-in [:cursor :col] start)
        (cursor-next-str re)
        (highlight-all-matches re))))

(defn move-back-same-word[b]
  (let [[word start] (word-under-cursor (:lines b) (:cursor b))
        re (str "\\b" word "\\b")]
    (swap! (:registers b) assoc "/" (str "?" re))
    (-> b 
        (assoc-in [:cursor :col] start)
        (cursor-back-str re)
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

(defn normal-mode-after[b keycode]
  (let [{col :col row :row} (:cursor b)
        lastbuf (-> b :context :lastbuf)]
    (if (not (nil? (motions-push-jumps keycode)))
      (jump-push lastbuf))
    (println "normal-mode-after, recording-keys" (-> b :macro :recording-keys))
    ;if nothing changed there is no need to overwrite "." register
    ;so keys like i<esc> won't affect, this also exclude all motions.
    (if (not (or (= (:lines lastbuf) (:lines b))
                 ;These commands should not get repeat
                 (contains? #{".", "u", "c+r", "p", "P", ":"} keycode)))
      (swap! (:registers b) assoc "." (-> b :macro :recording-keys)))
    ;alwasy clear :recording-keys
    ;prevent cursor on top of EOF in normal mode
    (let [b1 (if (and (> col 0) (>= col (dec (col-count b row))))
               (update-in b [:cursor :col] dec)
               b)]
      (-> b1 
          (assoc-in [:macro :recording-keys] [])
          (buf-update-highlight-brace-pair (:cursor b1))))))

(defn dot-repeat[b]
  (let [keycodes (@(:registers b) ".")]
    (if (empty? keycodes)
      b
      (let [{in :chan-in out :chan-out} b] 
        ;remove "." from normal-mode-keymap prevent recursive, probably useless
        (replay-keys b keycodes (dissoc @normal-mode-keymap "."))))))

(defn cut-char[b]
  (if (= (col-count b (-> b :cursor :row)) 1)
    b
    (let [cursor (:cursor b)]
      (swap! (:registers b) assoc "\"" (buf-copy-range b cursor cursor true))
      (-> b 
          buf-save-cursor
          (buf-replace cursor "" true)
          history-save))))

(defn yank-visual[b]
  (let [[p1 p2] (-> b :visual :ranges)]
    (swap! (:registers b) assoc "\"" (buf-copy-range b p1 p2 false))
    (let [[cur1 _] (apply cursor-sort (-> b :visual :ranges))]
      (-> b 
          (assoc :cursor {:row (:row cur1)
                          :col (:col cur1)
                          :lastcol (:col cur1)
                          :vprow (bound-range 
                                   (+ (-> b :cursor :vprow) 
                                      (- (:row cur1) (-> b :cursor :row)))
                                   0 
                                   (-> @window :viewport :h))})))))

(defn cursor-bound-vprow[cur]
  (update-in cur [:vprow]
             bound-range 
             0 
             (-> @window :viewport :h)))

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
                  newcur (cursor-bound-vprow (pos :cursor))]
              (if (= newid @active-buffer-id) 
                ;update pos inside current buffer
                (assoc b :cursor newcur)
                (let []
                  (reset! active-buffer-id newid)
                  (swap! buffer-list assoc-in [newid :cursor] newcur)
                  b)))
            ;buffer has been modifed and cursor is no longer inside, ignore
            (recur (fndir b))))))))

(defn move-next-empty-line[b]
  (let [b1 (cursor-simple-next-str b "^[^\n]")
        b2 (cursor-simple-next-str b1 "^\n") ]
    (if (or (= b b1) (= b1 b2))
      b
      b2)))

(defn move-back-empty-line[b]
  (let [b1 (cursor-simple-back-str b "^[^\n]")
        b2 (cursor-simple-back-str b1 "^\n") ]
    (if (or (= b b1) (= b1 b2))
      b
      b2)))

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
          {"h" #(cursor-move-char % 0)
           "l" #(cursor-move-char % 1)
           "k" #(cursor-move-char % 2)
           "j" #(cursor-move-char % 3)
           "g" {"g" cursor-move-start}
           "G" cursor-move-end
           "w" buf-line-next-word
           "W" buf-line-next-WORD
           "b" cursor-back-word
           "B" cursor-back-WORD
           "e" cursor-word-end
           "E" cursor-WORD-end
           "0" cursor-line-first
           "^" cursor-line-start
           "$" cursor-line-end
           "f" { :else move-to-next-char }
           "F" { :else move-to-back-char }
           "t" { :else move-before-next-char }
           "T" { :else move-after-back-char }
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
           "n" #(let[s (or (@(:registers %) "/") "/")
                     dir (first s)
                     re (subs s 1)
                     fnsearch (if (= \/ dir) cursor-next-str cursor-back-str)]
                  (-> % 
                      (fnsearch re)
                      (highlight-all-matches re)))
           "N" #(let[s (or (@(:registers %) "/") "?")
                     dir (or (first s) "")
                     re (subs s 1)
                     fnsearch (if (= \/ dir) cursor-back-str cursor-next-str)]
                  (-> % 
                      (fnsearch re)
                      (highlight-all-matches re)))
           "}" move-next-empty-line
           "{" move-back-empty-line
           "%" (fn[b]
                 (let [b1 (if (not (contains? all-braces (char-under-cursor b)))
                            (cursor-next-re b #"[\(\[\{\)\]\}]" #"[\(\[\{\)\]\}]")
                            b)]
                   (let [cur (cursor-match-brace b1)]
                     (if (nil? cur) 
                       b1
                       (let [newcur (-> cur 
                                        (assoc :lastcol (:col cur))
                                        (assoc :vprow (+ (-> b1 :cursor :vprow)
                                                         (- (:row cur) (-> b1 :cursor :row))))
                                        cursor-bound-vprow)]
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
           "c+r" { :else put-from-register }
           :else insert-mode-default 
           :enter set-insert-mode
           :continue #(not (= "esc" %2))
           :leave (fn[b keycode]
                    (-> b
                        (cursor-move-char 0)
                        set-normal-mode
                        history-save))})

  (reset! normal-mode-keymap @motion-keymap)
  (swap! normal-mode-keymap 
         merge 
         {"w" cursor-next-word
          "W" cursor-next-WORD

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
          "o" (merge
                @insert-mode-keymap
                {:enter set-insert-new-line})
          "." dot-repeat
          ":" (merge
                @ex-mode-keymap
                {"enter" execute
                 :enter (fn[b keycode] (set-ex-mode b))
                 :leave (fn[b keycode] (set-normal-mode b))})
          "u" history-undo
          "c+r" history-redo
          "c+o" #(move-to-jumplist % jump-prev)
          "c+i" #(move-to-jumplist % jump-next)
          "esc" set-normal-mode
          "v" (merge
                @visual-mode-keymap
                {:enter (fn[b keycode] (set-visual-mode b))
                 :leave (fn[b keycode]
                          (set-normal-mode b))
                 :continue #(not (or (= "esc" %2) (= "v" %2) (= "y" %2)))
                 :after visual-mode-select})
          "z" {"z" cursor-center-viewport }
          "d" (merge 
                @motion-keymap
                {:before save-lastbuf 
                 :after delete-motion
                 ;"j" delete-line
                 ;"k" delete-line
                 "d" delete-line})
          "x" cut-char
          "p" #(put-from-register-append % "\"")
          "P" #(put-from-register % "\"")
          "D" delete-line
          "J" (fn[b]
                (-> b 
                    buf-save-cursor
                    buf-join-line
                    history-save))
          "c" (merge
                @motion-keymap
                {:before save-lastbuf
                 :after change-motion})
          :before (fn [b keycode]
                    (assoc-in b [:context :lastbuf] b))
          :after normal-mode-after})
(reset! root-keymap @normal-mode-keymap))
