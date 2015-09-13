(ns webvim.keymap
  (:require [me.raynes.fs :as fs]
            [ring.adapter.jetty :as jetty]
            [clojure.core.async :as async])
  (:use clojure.pprint
        webvim.buffer
        webvim.cursor
        webvim.global
        webvim.jumplist
        webvim.ex
        webvim.serve
        webvim.change
        webvim.text
        webvim.line
        webvim.indent
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
      (assoc-in [:visual :ranges] [])
      (merge {:ex "" :mode insert-mode :message nil :keys nil})))

(defn set-insert-append[b keycode]
    (-> b
        char-forward
        (set-insert-mode keycode)))

(defn set-insert-line-end[b keycode]
  (-> b
      line-end
      (set-insert-mode keycode)))

(defn set-insert-line-start[b keycode]
  (-> b
      line-start
      (set-insert-mode keycode)))

(defn set-insert-remove-char[b keycode]
  (let [pos (b :pos)]
    (registers-put (b :registers) (-> b :context :register) (buf-copy-range b pos pos true))
    (-> b
        (set-insert-mode keycode)
        (text-delete-offset 1))))

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

(defn uncomplete-word
  [t]
  (let [pos (t :pos)
        s (t :str)]
    (if (zero? pos) nil
      (let [b (dec pos)
            a (or (first (pos-re-backward b s re-word-start-border)) b)]
        (str (text-subs s a b))))))

(defn keycode-to-char[keycode]
  (cond 
    (= 1 (count keycode))
    keycode
    (= "enter" keycode)
    "\n"
    (= "tab" keycode)
    "\t"
    (= "space" keycode)
    " "
    :else ""))

(defn insert-mode-default[t keycode]
  (let [t1 (if (= "backspace" keycode)
             (text-delete-offset t -1)
             (text-insert t (keycode-to-char keycode)))
        t2 (buf-update-highlight-brace-pair t1 (-> t1 :pos dec))
        t3 (if (or (re-test (-> t2 :language :indent-triggers) keycode) (= keycode "enter"))
             (-> t2 
                 buf-indent-current-line 
                 char-forward)
             t2)]
    (if (empty? (-> t3 :autocompl :suggestions))
      t3
      (let [word (uncomplete-word t3)
            suggestions (autocompl-suggest @autocompl-words word)]
        (if (= 1 (count suggestions))
          (assoc-in t3 [:autocompl :suggestions] [])
          (assoc t3 :autocompl 
                 (merge (:autocompl t3) 
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

(defn- buf-pos-info[b]
  (let [{nm :name 
         path :filepath
         y :y} b]
    (assoc b :message (str "\"" (or path nm) "\" line " (inc y)))))


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
  (registers-put (:registers b) (-> b :context :register) (buf-copy-range b cur (b :pos) inclusive))
  b)

(defn delete-motion[b keycode]
  ;(println (str "delete-motion:" keycode))
  ;(println (str "inclusive:" (inclusive? keycode)))
  (let [lastbuf (-> b :context :lastbuf)
        pos (b :pos)]
    (if (or (nil? lastbuf) (= pos (:pos lastbuf))) ;don't do anything if cursor doesn't change
      b
      (let [inclusive (inclusive? keycode)]
        (-> lastbuf
            (buf-copy-range-lastbuf pos inclusive)
            (text-delete (if inclusive (inc pos) pos))
            text-save-undo)))))

(defn change-motion[b keycode]
  ;(println "change-motion:" keycode)
  (let [lastbuf (-> b :context :lastbuf)
        lastpos (:pos lastbuf)
        pos (b :pos)]
    (if (or (nil? lastbuf) (= pos lastpos))
      b
      (let [inclusive (inclusive? keycode)]
        (-> lastbuf
            ;(println (str "change-motion:" keycode))
            (buf-copy-range-lastbuf lastpos inclusive)
            (text-delete (if inclusive (inc pos) (dec pos)))
            (serve-keymap (@normal-mode-keymap "i") keycode))))))

(defn yank-motion[b keycode]
  (let [lastbuf (-> b :context :lastbuf)
        lastpos (:pos lastbuf)
        pos (b :pos)]
    (if (or (nil? lastbuf) (= pos lastpos))
      b
      (let [inclusive (inclusive? keycode)]
        (buf-copy-range-lastbuf b lastpos inclusive)
        lastbuf))))

(defn indent-motion[b keycode]
  (let [lastbuf (-> b :context :lastbuf)
        lastpos (:pos lastbuf)
        pos (b :pos)]
    (if (or (nil? lastbuf) (= pos lastpos))
      b
      (-> b 
          (buf-indent-lines lastpos)
          text-save-undo))))

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

(defn autocompl-start[t]
  (let [pos (t :pos)
        word (uncomplete-word t)
        _ (println word)
        suggestions (autocompl-suggest @autocompl-words word)]
    (println "autocompl:" suggestions)
    (assoc t :autocompl 
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
        (if (empty? w) b
          (-> b1 
              (assoc-in [:autocompl :suggestions-index] n)
              (update-in [:macro :recording-keys] 
                         #(apply conj % ks)) 
              (buffer-replace-suggestion w)))))))

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
          (text-insert txt)
          dec-col
          text-save-undo)
      b)))

(defn put-from-register-append[b keycode]
  (let [txt (registers-get (:registers b) keycode)]
    (if (string? txt)
      (-> b
          (text-insert (b :pos) txt)
          text-save-undo)
      b)))

(defn delete-line[t]
  (let [pos (t :pos)
        [a b] (current-line t)]
    (registers-put (:registers t) (-> t :context :register) (buf-copy-range t a b false))
    (-> t
        (update-in [:context] dissoc :lastbuf) ;remove :lastbuf prevent delete-motion take over.
        (text-delete-range (current-line t))
        line-start
        text-save-undo)))

(defn delete-to-line-end[t]
  (let [pos (t :pos)
        [a b] (current-line t)]
    (registers-put (:registers t) (-> t :context :register) (buf-copy-range t pos b false))
    (-> t
      (update-in [:context] dissoc :lastbuf) ;remove :lastbuf prevent delete-motion take over.
      (text-delete a b)
      text-save-undo)))

(defn change-to-line-end[b]
  (-> b
      (text-delete (b :pos) (-> b current-line last dec))
      (serve-keymap (@normal-mode-keymap "i") "c")))

(defn delete-range[b]
  (-> b
      (text-delete-inclusive (b :pos) (-> b :context :lastbuf :pos))
      text-save-undo))

(defn change-range[b]
  (-> b 
      (text-delete-inclusive (b :pos) (-> b :context :lastbuf :pos))
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

(defn update-x[b]
  (let [pos (b :pos)]
    (assoc b :x (- pos (pos-line-first pos (b :str))))))

(defn update-x-if-not-jk
  "update :x unless it is up down motion"
  [b lastbuf keycode]
  (if-not (or (= (:pos lastbuf) (:pos b)) 
              (contains? #{"j" "k"} keycode))
    (update-x b) b))

(defn normal-mode-fix-pos
    "prevent cursor on top of EOL in normal mode"
    [b]
    (let [ch (text-char-at (b :str) (b :pos))]
      (if (= (or ch \newline) \newline)
        (char-backward b) b)))

(defn normal-mode-after[b keycode]
  (let [lastbuf (-> b :context :lastbuf)]
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
        (update-x-if-not-jk lastbuf keycode)
        ;alwasy clear :recording-keys
        (assoc-in [:macro :recording-keys] [])
        (buf-update-highlight-brace-pair (b :pos)))))

(defn dot-repeat[b]
  (let [keycodes (registers-get (:registers b) ".")]
    (if (empty? keycodes)
      b
      (let [{in :chan-in out :chan-out} b] 
        ;remove "." from normal-mode-keymap prevent recursive, probably useless
        (replay-keys b keycodes (dissoc @normal-mode-keymap "."))))))

(defn cut-char[b]
    (let [pos (b :pos)]
      (registers-put (:registers b) (-> b :context :register) (buf-copy-range b pos pos true))
      (-> b 
          (text-delete-offset 1)
          text-save-undo)))

(defn yank-visual[b]
  (let [[p1 p2] (-> b :visual :ranges)]
    (registers-put (:registers b) (-> b :context :register) (buf-copy-range b p1 p2 true))
    (let [[cur1 _] (apply sort2 (-> b :visual :ranges))]
      b)))

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
          (if (< (pos :pos) (count (newb :str)))
            (let [newid (newb :id)
                  newpos (pos :pos)]
              (if (= newid @active-buffer-id) 
                ;update pos inside current buffer
                (text-update-pos b newpos)
                (let []
                  (change-active-buffer newid)
                  (swap! buffer-list update-in [newid] #(text-update-pos % newpos))
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
          {"h" char-backward
           "l" char-forward
           "k" #(lines-backward % 1)
           "j" #(lines-forward % 1)
           "g" {"g" text-start}
           ;"G" cursor-move-end
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
                "enter" nop
                :else move-to-next-char }
           "F" {"esc" nop
                "enter" nop
                :else move-to-back-char }
           "t" {"esc" nop 
                "enter" nop
                :else move-before-next-char }
           "T" {"esc" nop 
                "enter" nop
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
                 (let [s (b :str)
                       pos (b :pos)
                       newpos (pos-match-brace s
                                (first (pos-re-forward pos s #"\(|\)|\[|\]|\{|\}")))]
                   (text-update-pos b newpos)))
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
                        update-x
                        set-normal-mode
                        text-save-undo))})

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
                         buf-indent-current-line
                         text-save-undo)
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
                             (buf-replace-char "\n")
                             buf-indent-current-line
                             text-save-undo))
               :else (fn[b keycode]
                       (let [ch (cond
                                  (= keycode "space")
                                  " "
                                  :else keycode)]
                         (if (= (count ch) 1)
                           (-> b 
                               (buf-replace-char ch)
                               text-save-undo)
                               b)))}
          "u" text-undo
          "c+r" text-redo
          "c+o" #(move-to-jumplist % jump-prev)
          "c+i" #(move-to-jumplist % jump-next)
          "c+g" buf-pos-info
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
                              (update-x-if-not-jk (b :lastbuf) keycode)))
                 "d" delete-range
                 "c" change-range
                 "o" (fn[b]
                       (let [[pt1 pt2] (-> b :visual :ranges)
                             newb (-> b 
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
          "C" change-to-line-end
          "J" (fn[b]
                (-> b 
                    buf-join-line
                    text-save-undo))
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

