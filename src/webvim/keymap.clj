(ns webvim.keymap
  (:require [ring.adapter.jetty :as jetty]
            [clojure.core.async :as async])
  (:use clojure.pprint
        webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.buffer
        webvim.core.serve
        webvim.core.line
        webvim.action.motion
        webvim.action.window
        webvim.action.edit
        webvim.action.mode
        webvim.action.macro
        webvim.action.visual
        webvim.action.highlight
        webvim.register
        webvim.jumplist
        webvim.utils
        webvim.ex
        webvim.indent
        webvim.autocompl))

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


(defn insert-mode-default[t keycode]
  (let [t1 (if (= "backspace" keycode)
             (buf-delete-offset t -1)
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

(defn start-register[b keycode]
  (let [m (re-find #"[0-9a-zA-Z/*#%.:+=\-]" keycode)]
    (if (not (nil? m))
      (-> b 
          (assoc-in [:context :register] keycode)
          (serve-keymap @normal-mode-keymap keycode)))))

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

(defonce ^{:private true} listen-change-buffer
  (listen 
    :change-buffer
    (fn [newt oldt c]
      (let [re (-> newt :registers deref (get "/" "/") (subs 1) re-pattern)]
        (if-not (or (-> newt :highlights empty?) (-> re str empty?))
          (highlight-all-matches newt re)
          newt)))))


(defn update-x[b]
  (let [pos (b :pos)]
    (assoc b :x (- pos (pos-line-first (b :str) pos)))))

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
           "g" {"g" buf-start}
           "G" buf-end
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
           "%" (fn[buf]
                 (buf-move buf
                           (fn [r pos]
                             (pos-match-brace 
                               r
                               (first (pos-re+ r pos #"\(|\)|\[|\]|\{|\}"))))))
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

;TODO: duplicate buf-bound-scroll-top
(defn- buf-bound-scroll-top
  "Change scroll top make cursor inside viewport"
  [b]
  (let [st (-> b :scroll-top)]
    (assoc b :scroll-top 
           (let [y (b :y)
                 h (-> @window :viewport :h)]
             (cond 
               (< y st) y
               (< y (+ st h)) st
               (neg? (-> y (- h) inc)) 0
               :else (-> y (- h) inc))))))

(defonce ^{:private true} listen-new-buffer
  (listen :new-buffer
          (fn [buf]
            (-> buf
                (assoc :before-send-out buf-bound-scroll-top)
                (assoc :after-send-out #(assoc % :changes []))))))
