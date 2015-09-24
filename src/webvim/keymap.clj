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
            :visual {:type 0 :ranges nil}
            :autocompl {:suggestions nil 
                        :suggestions-index 0}}))

(defn set-visual-mode[b]
  ;(println "set-visual-mode:")
  (let [pos (b :pos)]
    (merge b {:ex "" :mode visual-mode :keys nil 
              :visual {:type 0 :ranges [[pos pos]]}})))

(defn set-insert-mode[b keycode]
  ;(println "set-insert-mode")
  (-> b 
      (assoc-in [:visual :ranges] nil)
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

(defn- insert-line-after[buf]
  (let [pos (buf :pos)
        [_ b] (current-line buf)]
    (-> buf
        (buf-insert b <br>)
        (buf-set-pos b))))

(defn set-insert-new-line[buf keycode]
  (-> buf 
      (set-insert-mode keycode)
      insert-line-after
      buf-indent-current-line))

(defn- insert-line-before[buf]
  (let [pos (buf :pos)
        [a b] (current-line buf)]
    (if (zero? a)
      (-> buf
          (buf-insert 0 <br>)
          (buf-set-pos 0))
      (-> buf
          (buf-set-pos (- a 1))
          (buf-insert <br>)))))

(defn set-insert-new-line-before[buf keycode]
  (-> buf 
      (set-insert-mode keycode)
      insert-line-before
      buf-indent-current-line))

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
             (buf-insert t (keycode-to-char keycode)))
        t2 (buf-update-highlight-brace-pair t1 (-> t1 :pos dec))
        t3 (if (or (re-test (-> t2 :language :indent-triggers) keycode) (= keycode "enter"))
             (buf-indent-current-line t2)
             t2)]
    (if (empty? (-> t3 :autocompl :suggestions))
      t3
      (let [word (uncomplete-word t3)
            suggestions (autocompl-suggest word)]
        (if (= 1 (count suggestions))
          (assoc-in t3 [:autocompl :suggestions] [])
          (assoc t3 :autocompl 
                 (merge (:autocompl t3) 
                        {:suggestions suggestions
                         :suggestions-index 0})))))))

(defn- buf-pos-info[b]
  (let [{nm :name 
         path :filepath
         y :y} b]
    (assoc b :message (str "\"" (or path nm) "\" line " (inc y)))))

(defn handle-search[b]
  (registers-put (:registers b) "/" (b :ex))
  (-> b
      (highlight-all-matches (re-pattern (str "(?m)" (subs (b :ex) 1))))
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
            (buf-delete (if inclusive (inc pos) pos))
            save-undo)))))

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
            (buf-delete (if inclusive (inc pos) pos))
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
          save-undo))))

(defn start-register[b keycode]
  (let [m (re-find #"[0-9a-zA-Z/*#%.:+=\-]" keycode)]
    (if (not (nil? m))
      (-> b 
          (assoc-in [:context :register] keycode)
          (serve-keymap @normal-mode-keymap keycode)))))

(defn visual-mode-select[t keycode]
  (let [m (re-find #"[ocdy]" keycode)] ;don't change cursor position if not motion
    (if (nil? m)
      (let [pos (t :pos)]
        (update-in t [:visual :ranges 0] 
                   (fn[[a b]] [pos b]))) t)))

(defn autocompl-start[t]
  (let [pos (t :pos)
        word (uncomplete-word t)
        suggestions (autocompl-suggest word)]
    ;(println "autocompl:" suggestions)
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
          (buf-insert txt)
          char-backward
          save-undo)
      b)))

(defn put-from-register-append[b keycode]
  (let [txt (registers-get (:registers b) keycode)]
    (if (string? txt)
      (let [pos (b :pos)]
        (-> b
            (buf-insert (inc pos) txt)
            (buf-set-pos (+ pos (count txt)))
            save-undo))
      b)))

(defn delete-line[t]
  (let [pos (t :pos)
        [a b] (current-line t)]
    (registers-put (:registers t) (-> t :context :register) (buf-copy-range t a b false))
    (-> t
        (update-in [:context] dissoc :lastbuf) ;remove :lastbuf prevent delete-motion take over.
        (text-delete-range (current-line t))
        line-start
        save-undo)))

(defn delete-to-line-end[t]
  (let [pos (t :pos)
        [a b] (current-line t)]
    (registers-put (:registers t) (-> t :context :register) (buf-copy-range t pos b false))
    (-> t
      (update-in [:context] dissoc :lastbuf) ;remove :lastbuf prevent delete-motion take over.
      (buf-delete a b)
      save-undo)))

(defn change-to-line-end[b]
  (-> b
      (buf-delete (b :pos) (-> b current-line last dec))
      (serve-keymap (@normal-mode-keymap "i") "c")))

(defn- delete-inclusive
  [t a b]
  (let [[a b] (sort2 a b)]
    (-> t
        (buf-delete a (inc b))
        (buf-set-pos a))))

(defn delete-range[t]
  (let [[a b] (-> t :visual :ranges first)]
    (-> t
        (delete-inclusive a b)
        save-undo)))

(defn change-range[t]
  (let [[a b] (-> t :visual :ranges first)]
    (-> t
        (delete-inclusive a b)
        (set-insert-mode "c")
        (serve-keymap (@normal-mode-keymap "i") "c"))))

(def left-boundary (str "(?<=^|[" not-word-chars "])"))
(def right-boundary (str "(?=[" not-word-chars "]|$)"))

;(pos-re-forward 0 (rope "a b") (re-pattern left-boundary))
;(pos-re-forward 0 (rope "a b") (re-pattern right-boundary))
;(pos-re-forward 0 (rope "abc bc") (re-pattern (str left-boundary "bc" right-boundary)))

(defn move-next-same-word[b]
  (let [[start end] (current-word b)
        word (subr (b :str) start end)
        ;_ (println (str word))
        re (re-pattern (str left-boundary (quote-pattern word) right-boundary))]
    (registers-put (:registers b) "/" (str "/" re))
    (-> b 
        (re-forward-highlight re)
        (highlight-all-matches re))))

(defn move-back-same-word[b]
  (let [[start end] (current-word b)
        word (subr (b :str) start end)
        re (re-pattern (str left-boundary (quote-pattern word) right-boundary))]
    (registers-put (:registers b) "/" (str "?" re))
    (-> b 
        (re-backward-highlight re)
        (highlight-all-matches re))))

;TODO: only highlight diff parts
;(defonce listen-change-buffer
;  (fn [newt oldt c]
;    (let [a1 (c :pos)
;          b1 (+ a1 (c :len))
;          highlights (oldt :highlights) 
;          {insects :insects 
;           not-insects :not-insects} (partition-by 
;                                       (fn[[a b]] 
;                                         (if (or (<= a a1 b) 
;                                                 (<= a b1 b))
;                                           :insects 
;                                           :not-insects)) highlights)
;          newhighlights (reduce 
;                          (fn [[a b]]
;                            ) nil insects)]
;      (assoc newt
;             :highlights (cons not-insects newhighlights)))))

(defonce ^{:private true} listen-change-buffer
  (listen 
    :change-buffer
    (fn [newt oldt c]
      (let [re (-> newt :registers deref (get "/" "/") (subs 1) re-pattern)]
        (if-not (or (-> newt :highlights empty?) (-> re str empty?))
          (highlight-all-matches newt re)
          newt)))))


(defn buf-close-chan-in[b]
  (async/close! (:chan-in b))
  b)

(defn replay-keys [b keycodes keymap]
  (let [b1 (-> b
               (assoc :registers (atom @(:registers b))) ;Use different reigsters when replay keys, avoid changing to the global registers.
               (assoc :chan-in (async/chan)) ;use local :chan-in :chan-out only for replay keys
               (assoc :chan-out (async/chan)))]
    ;(println "start replay:")
    ;(pprint keycodes)
    (key-server b1 keymap)
    (let [b2 (reduce 
               (fn[b1 kc]
                 (let[_ (async/>!! (b1 :chan-in) kc) 
                      b2 (async/<!! (b1 :chan-out))]
                   (assoc b2 
                          :changes
                          (concat (b1 :changes) (b2 :changes))))) b1 keycodes)]
      (-> b2
          buf-close-chan-in
          ;restore back
          (assoc :registers (b :registers))
          (assoc :chan-in (b :chan-in))
          (assoc :chan-out (b :chan-out))))))

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
    (let [ch (char-at (b :str) (b :pos))]
      (if (= (or ch \newline) \newline)
        (char-backward b) b)))

(defn normal-mode-after[b keycode]
  (let [lastbuf (-> b :context :lastbuf)]
    (if-not (nil? (motions-push-jumps keycode))
      (jump-push lastbuf))
    ;(println "normal-mode-after, recording-keys" (-> b :macro :recording-keys))
    ;if nothing changed there is no need to overwrite "." register
    ;so keys like i<esc> won't affect, this also exclude all motions.
    (if-not (or (= (:str lastbuf) (:str b))
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
      ;remove "." from normal-mode-keymap prevent recursive, probably useless
      (replay-keys b keycodes (dissoc @normal-mode-keymap ".")))))

(defn cut-char[b]
    (let [pos (b :pos)]
      (registers-put (:registers b) (-> b :context :register) (buf-copy-range b pos pos true))
      (-> b 
          (text-delete-offset 1)
          save-undo)))

(defn yank-visual[t]
  (let [[a b] (-> t :visual :ranges first)]
    (registers-put (:registers t) (-> t :context :register) (buf-copy-range t a b true))
    (let [[newpos  _] (sort2 a b)]
      (buf-set-pos t newpos))))

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
                (buf-set-pos b newpos)
                (let []
                  (change-active-buffer newid)
                  (swap! buffer-list update-in [newid] #(buf-set-pos % newpos))
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
           "G" text-end
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
           "n" (fn[b]
                 (let[s (or (registers-get (:registers b) "/") "/")
                      dir (first s)
                      re (re-pattern (str "(?m)" (subs s 1)))
                      hightlightall? (-> b :highlights empty?)
                      fnsearch (if (= \/ dir) re-forward-highlight re-backward-highlight)
                      b1 (fnsearch b re)] ;TODO: 1. no need fnsearch if highlight all matches. 2. cache highlight-all-matches
                   (if hightlightall?
                     (highlight-all-matches b1 re) b1)))
           "N" (fn[b]
                 (let[s (or (registers-get (:registers b) "/") "?")
                      dir (or (first s) "")
                      re (re-pattern (str "(?m)" (subs s 1)))
                      hightlightall? (-> b :highlights empty?)
                      fnsearch (if (= \/ dir) re-backward-highlight re-forward-highlight)
                      b1 (fnsearch b re)]
                   (if hightlightall?
                     (highlight-all-matches b1 re) b1)))
           "}" paragraph-forward
           "{" paragraph-backward
           "%" (fn[b]
                 (let [s (b :str)
                       pos (b :pos)
                       newpos (pos-match-brace s
                                (first (pos-re-forward pos s #"\(|\)|\[|\]|\{|\}")))]
                   (buf-set-pos b newpos)))
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
                        save-undo))})

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
                         save-undo)
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
                             save-undo))
               :else (fn[b keycode]
                       (let [ch (cond
                                  (= keycode "space")
                                  " "
                                  :else keycode)]
                         (if (= (count ch) 1)
                           (-> b 
                               (buf-replace-char ch)
                               save-undo)
                               b)))}
          "u" undo
          "c+r" redo
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
                 "o" (fn[buf]
                       (let [[a b] (-> buf :visual :ranges first)
                             newt (-> buf
                                      (assoc-in [:visual :ranges 0] [b a])
                                      (buf-set-pos b))]
                             (assoc-in newt [:context :lastbuf] newt)))})
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
                    save-undo))
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

