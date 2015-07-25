(ns webvim.keymap
  (:require [ring.adapter.jetty :as jetty]
            [clojure.core.async :as async]
            [ring.util.response :as response])
  (:use clojure.pprint
        webvim.buffer
        webvim.autocompl))

(defonce motion-keymap (atom {}))
(defonce edit-keymap (atom {}))
(defonce normal-mode-keymap (atom {}))
(defonce visual-mode-keymap (atom {}))
(defonce insert-mode-keymap (atom {}))
(defonce ex-mode-keymap (atom {}))

;enter point of key sequence parser
(defonce root-keymap (atom {}))
(defonce registers (atom {"%" (:name @active-buffer)}))

(defonce normal-mode 0)
(defonce insert-mode 1)
(defonce visual-mode 2)

(declare serve-keymap)
(declare map-fn-else)

(defn call-if-fn [b f & args]
  (if (fn? f)
    (apply f b args)
    b))

(defn- pprint2[obj prefix]
  (let[]
    (println prefix)
    (pprint obj)
    obj))


;two types: key (leaf), keymap (internal node)
;when visit a keymap call :enter :leave 
; keymap is repetitive if :continue return true
;when visit a key call :before :after
(defn serve-keys
  "Serve a sequence of keys until end of keymap. Recusivly walk through keymap tree (works like sytax parser)"
  [b keymap keycode]
  (let [f (keymap keycode)]
    (println "got key:" keycode)
    (let [b1 (update-in b [:macro :recording-keys] #(conj % keycode))]
      (if (or (fn? f) (map? f) (fn? (:else keymap)))
        (-> b1
            (call-if-fn (:before keymap) keycode)
            (map-fn-else keymap keycode)
            (call-if-fn (:after keymap) keycode))
        b1))))

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
        (recur (key-server-inner b1 keymap))))))

(defn set-normal-mode[b]
  (print "set-normal-mode:")
  (merge b {:ex "" 
            :mode normal-mode 
            :keys nil 
            :autocompl {:suggestions nil 
                        :suggestions-index 0 
                        :words (-> b :autocompl :words)}}))

(defn set-visual-mode[b]
  (print "set-visual-mode:")
  (let [cur (-> b :cursor cursor-to-point)]
    (merge b {:ex "" :mode visual-mode :keys nil 
              :visual {:type 0 :ranges [cur cur]}})))

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
  (swap! registers assoc "\"" (buf-copy-range b (:cursor b) (:cursor b) true))
  (-> b
      (set-insert-mode keycode)
      (buf-replace (:cursor b) "" true)))

(defn set-insert-new-line[b keycode]
  (-> b 
      (set-insert-mode keycode)
      buf-insert-line-after))

(defn set-ex-mode[b]
  (merge b {:ex ":" :message nil :keys nil}))

(defn set-ex-search-mode[b keycode]
  (println "set-ex-search-mode")
  (-> b 
      (merge {:ex "/" :message nil :keys nil})
      (assoc-in [:context :lastbuf] b)))

(defn insert-mode-default[b keycode]
  (println (str "insert-mode-default:" keycode))
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
             b)]
    (if (empty? (-> b1 :autocompl :suggestions))
      b1
      (let [word (buffer-word-before-cursor b1)
            suggestions (-> b1 :autocompl :words (autocompl-suggest word))]
        (if (= 1 (count suggestions))
          (assoc-in b1 [:autocompl :suggestions] [])
          (assoc b1 :autocompl 
                 (merge (:autocompl b1) 
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
    (if (= \/ (first newex))
      (let [lb (-> b :context :lastbuf)
            newb (-> lb
                     (assoc :ex newex)
                     (assoc-in [:context :lastbuf] lb))] ;keep lastbuf avaiable on stack
        (try (cursor-next-str newb (subs newex 1))
             (catch Exception e  newb)))
      (assoc b :ex newex))))

(defn execute [b]
  (let [ex (:ex b)]
    (cond
      (= ex ":w")
      (write-buffer b)
      (= ex ":e")
      (merge (open-file (:name b)) 
             {:cursor (:cursor b) 
              :message (str "\"" (:name b) "\" " (count (:lines b)) "L")})
      (string? (re-find #":\d+" ex))
      (let [row (bound-range (dec (Integer. (re-find #"\d+" ex))) 0 (-> b :lines count dec))]
        (-> b 
            (assoc-in [:cursor :row] row)
            (assoc-in [:cursor :vprow] (-> @window :viewport :h (/ 2) int))
            (cursor-line-start)))
      (= \/ (first ex))
      (let []
        (swap! registers assoc "/" (subs ex 1))
        b)
      :else
      (assoc b :message "unknown command"))))

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
   "/" false})

(defn inclusive? [keycode]
  (let [res (map-key-inclusive keycode)]
    (if (nil? res)
      true
      res)))

(defn- buf-copy-range-lastbuf[b cur inclusive]
  (println (str "inclusive:" inclusive))
  (swap! registers assoc "\"" (buf-copy-range b cur (:cursor b) inclusive))
  b)

(defn delete-motion[b keycode]
  (println (str "delete-motion:" keycode))
  (println (str "inclusive:" (inclusive? keycode)))
  (if (nil? (-> b :context :lastbuf))
    b
    (-> b :context :lastbuf 
        buf-save-cursor
        (buf-copy-range-lastbuf (:cursor b) (inclusive? keycode))
        (buf-replace
          (:cursor b) "" (inclusive? keycode))
        history-save)))

(defn change-motion[b keycode]
  (println "change-motion:" keycode)
  (-> b :context :lastbuf
      ;(println (str "change-motion:" keycode))
      (buf-copy-range-lastbuf (:cursor b) (inclusive? keycode))
      (buf-replace (:cursor b) "" (inclusive? keycode))
      (serve-keymap (@normal-mode-keymap "i") keycode)))

(defn visual-mode-select[b keycode]
  (print "visual-mode-select:")
  (assoc-in b [:visual :ranges 1]
            (-> b :cursor cursor-to-point)))

(defn autocompl-start[b]
  (let [word (buffer-word-before-cursor b)
        suggestions (autocompl-suggest (-> b :autocompl :words) word)]
    (if (= 1 (count suggestions))
      (assoc-in b [:autocompl :suggestions] [])
      (assoc b :autocompl 
             (merge (:autocompl b) 
                    {:suggestions suggestions 
                     :suggestions-index 0})))))

(defn autocompl-move[b f]
  (let [b1 (if (empty? (-> b :autocompl :suggestions))
             (autocompl-start b)
             b)
        i (f (-> b1 :autocompl :suggestions-index))
        cnt (-> b1 :autocompl :suggestions count)]
    (if (zero? cnt)
      b1
      (let [n (mod (+ i cnt) cnt)]
        (-> b1 
            (assoc-in [:autocompl :suggestions-index] n)
            (buffer-replace-suggestion (-> b1 :autocompl :suggestions (get n))))))))

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

(defn put-from-register[b keycode]
  (let [txt (@registers keycode)]
    (if (string? txt)
      (-> b
          buf-save-cursor
          (buf-insert txt)
          (cursor-move-char 0)
          history-save)
      b)))

(defn put-from-register-append[b keycode]
  (let [txt (@registers keycode)]
    (if (string? txt)
      (-> b
          buf-save-cursor
          (update-in [:cursor :col] inc)
          (buf-insert txt)
          (cursor-move-char 0)
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
    (swap! registers assoc "/" re)
    (-> b 
        (assoc-in [:cursor :col] start)
        (cursor-next-str re))))

(defn move-back-same-word[b]
  (let [[word start] (word-under-cursor (:lines b) (:cursor b))
        re (str "\\b" word "\\b")]
    (swap! registers assoc "/" re)
    (-> b 
        (assoc-in [:cursor :col] start)
        (cursor-back-str re))))

(defn buf-close-chan-in[b]
  (async/close! (:chan-in b))
  b)

(defn replay-keys [b keycodes keymap]
  (let [in (async/chan) ;use local :chan-in :chan-out only for replay keys
        out (async/chan)
        b1 (-> b
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
              ;restore channel back
              (assoc :chan-in (:chan-in b))
              (assoc :chan-out (:chan-out b)))
          (recur (first coll) (subvec coll 1)))))))

(defn normal-mode-after[b keycode]
  (let [{col :col row :row} (:cursor b)
        lastbuf (-> b :context :lastbuf)]
    ;if nothing changed there is no need to overwrite "." register
    ;so keys like i<esc> won't affect, this also exclude all motions.
    (if (not (or (= (:lines lastbuf) (:lines b))
                 ;These commands should not get repeat
                 (contains? #{".", "u", "c+r", "p", "P", ":"} keycode)))
      (swap! registers assoc "." (-> b :macro :recording-keys)))
    ;alwasy clear :recording-keys
    ;prevent cursor on top of EOF in normal mode
    (if (and (> col 0) (>= col (dec (col-count b row))))
      (-> b 
          (update-in [:cursor :col] dec)
          (assoc-in [:macro :recording-keys] []))
      (assoc-in  b [:macro :recording-keys] []))))

(defn dot-repeat[b]
  (let [keycodes (@registers ".")]
    (if (empty? keycodes)
      b
      (let [{in :chan-in out :chan-out} b] 
        ;remove "." from normal-mode-keymap prevent recursive, probably useless
        (replay-keys b keycodes (dissoc @normal-mode-keymap "."))))))

(defn cut-char[b]
  (if (= (col-count b (-> b :cursor :row)) 1)
    b
    (let [cursor (:cursor b)]
      (swap! registers assoc "\"" (buf-copy-range b cursor cursor true))
      (-> b 
          buf-save-cursor
          (buf-replace cursor "" true)
          history-save))))

(defn yank-visual[b]
  (let [[p1 p2] (-> b :visual :ranges)]
    (swap! registers assoc "\"" (buf-copy-range b p1 p2 true))
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

(defn init-keymap-tree
  []
  (reset! ex-mode-keymap
          {"enter" execute
           :continue #(not (or (= "esc" %2) (= "enter" %2) (empty? (:ex %1))))
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
           "w" cursor-next-word
           "W" cursor-next-WORD
           "b" cursor-back-word
           "B" cursor-back-WORD
           "e" cursor-word-end
           "E" cursor-WORD-end
           "0" cursor-line-first
           "^" cursor-line-start
           "$" cursor-line-end
           "f" { :else move-to-next-char }
           "F" { :else move-to-back-char }
           "t" { :else #(-> %1 
                            (update-in [:keys] conj %2)
                            (move-to-next-char %2)
                            (cursor-move-char 0)) }
           "T" { :else #(-> %1
                            (move-to-back-char %2)
                            (cursor-move-char 1)) }
           "/" (merge @ex-mode-keymap 
                      {:enter set-ex-search-mode
                       :else ex-mode-default})
           "*" move-next-same-word
           "#" move-back-same-word
           "n" #(cursor-next-str % (@registers "/"))
           "N" #(cursor-back-str % (@registers "/"))
           "}" (fn[b]
                 (-> b
                     (cursor-next-str "^[^\n]")
                     (cursor-next-str "^\n")))
           "{" (fn[b]
                 (-> b
                     (cursor-back-str "^[^\n]")
                     (cursor-back-str "^\n")))
           "%" (fn[b]
                 (let [b1 (if (not (contains? all-braces (char-under-cursor b)))
                            (cursor-next-str b "[(\\[{}\\])]")
                            b)]
                   (let [cur (cursor-match-brace b1)]
                     (if (nil? cur) b1
                       (update-in b1 [:cursor] 
                                  merge cur 
                                  {:lastcol (:col cur)} 
                                  {:vprow (bound-range 
                                            (+ (-> b1 :cursor :vprow) (- (:row cur) (-> b1 :cursor :row))) 
                                            0 (-> @window :viewport :h dec))})))))
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
         {"i" @insert-mode-keymap
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
                {:enter (fn[b keycode] (set-ex-mode b))
                 :leave (fn[b keycode] (set-normal-mode b))})
          "u" history-undo
          "c+r" history-redo
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
          "c" (merge
                @motion-keymap
                {:before save-lastbuf 
                 :after change-motion })
          :before (fn [b keycode]
                    (assoc-in b [:context :lastbuf] b))
          :after normal-mode-after})
(reset! root-keymap @normal-mode-keymap))
