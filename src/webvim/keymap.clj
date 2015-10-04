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
        webvim.core.register
        webvim.jumplist
        webvim.utils
        webvim.ex
        webvim.indent
        webvim.autocompl))

(defn change-motion[buf keycode]
  ;(println "change-motion:" keycode)
  (let [lastbuf (-> buf :context :lastbuf)
        lastpos (:pos lastbuf)
        pos (buf :pos)]
    (if (or (nil? lastbuf) (= pos lastpos))
      buf
      (let [inclusive (inclusive? keycode)]
        (-> lastbuf
            ;(println (str "change-motion:" keycode))
            (buf-copy-range-lastbuf lastpos inclusive)
            (buf-delete (if inclusive (inc pos) pos))
            (serve-keymap (-> buf :root-keymap (get "i")) keycode))))))

(defn insert-mode-default[t keycode]
  (let [t1 (if (= "<bs>" keycode)
             (buf-delete-offset t -1)
             (buf-insert t (keycode-to-char keycode)))
        t2 (buf-update-highlight-brace-pair t1 (-> t1 :pos dec))
        t3 (if (or (re-test (-> t2 :language :indent-triggers) keycode) (= keycode "<cr>"))
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

(defn start-register[buf keycode]
  (let [m (re-find #"[0-9a-zA-Z/*#%.:+=\-]" keycode)]
    (if (not (nil? m))
      (-> buf 
          (assoc-in [:context :register] keycode)
          (serve-keymap (-> buf :root-keymap (get "i")) keycode)))))

(defn autocompl-start[t]
  (let [pos (t :pos)
        word (uncomplete-word t)
        suggestions (autocompl-suggest word)]
    ;(println "autocompl:" suggestions)
    (assoc t :autocompl 
           {:suggestions suggestions 
            :suggestions-index 0})))

(defn autocompl-move[buf f]
  (let [b1 (if (empty? (-> buf :autocompl :suggestions))
             (autocompl-start buf)
             buf)
        i (f (-> b1 :autocompl :suggestions-index))
        cnt (-> b1 :autocompl :suggestions count)]
    (if (zero? cnt)
      b1
      (let [n (mod (+ i cnt) cnt)
            w (-> b1 :autocompl :suggestions (get n))
            s (-> b1 :autocompl :suggestions (get 0))
            ;delete back then insert word
            ks (apply conj (vec (repeat (count s) "<bs>")) (map str (vec w)))]
        (if (empty? w) buf
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


(defn update-x[buf]
  (let [pos (buf :pos)]
    (assoc buf :x (- pos (pos-line-first (buf :str) pos)))))

(defn update-x-if-not-jk
  "update :x unless it is up down motion"
  [buf lastbuf keycode]
  (if-not (or (= (:pos lastbuf) (:pos buf)) 
              (contains? #{"j" "k"} keycode))
    (update-x buf) buf))

(defn normal-mode-fix-pos
    "prevent cursor on top of EOL in normal mode"
    [buf]
    (let [ch (char-at (buf :str) (buf :pos))]
      (if (= (or ch \newline) \newline)
        (char-backward buf) buf)))

(defn normal-mode-after[buf keycode]
  (let [lastbuf (-> buf :context :lastbuf)]
    (if-not (nil? (motions-push-jumps keycode))
      (jump-push lastbuf))
    ;(println "normal-mode-after, recording-keys" (-> buf :macro :recording-keys))
    ;if nothing changed there is no need to overwrite "." register
    ;so keys like i<esc> won't affect, this also exclude all motions.
    (if-not (or (= (:str lastbuf) (:str buf))
                 ;These commands should not get repeat
                 (contains? #{".", "u", "<c+r>", "p", "P", ":"} keycode))
      (registers-put (:registers buf) "." (-> buf :macro :recording-keys)))
    (-> buf 
        normal-mode-fix-pos
        (update-x-if-not-jk lastbuf keycode)
        ;alwasy clear :recording-keys
        (assoc-in [:macro :recording-keys] [])
        (buf-update-highlight-brace-pair (buf :pos)))))

(defn dot-repeat[buf]
  (let [keycodes (registers-get (:registers buf) ".")]
    (if (empty? keycodes)
      buf
      ;remove "." from @root-keymap prevent recursive, probably useless
      (replay-keys buf keycodes (-> buf :root-keymap (dissoc "."))))))

(defn init-motion-keymap[ex-mode-keymap]
  {"h" char-backward
   "l" char-forward
   "k" #(lines-n- % 1)
   "j" #(lines-n+ % 1)
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
   "f" {"<esc>" identity
        "<cr>" identity
        :else move-to-next-char }
   "F" {"<esc>" identity
        "<cr>" identity
        :else move-to-back-char }
   "t" {"<esc>" identity 
        "<cr>" identity
        :else move-before-next-char }
   "T" {"<esc>" identity 
        "<cr>" identity
        :else move-after-back-char }
   "/" (merge ex-mode-keymap ;TODO: This is not ex-mode
              {"<cr>" handle-search
               :enter set-ex-search-mode
               :else ex-mode-default})
   "?" (merge ex-mode-keymap
              {"<cr>" handle-search
               :enter set-ex-search-mode
               :else ex-mode-default})
   "*" move-next-same-word
   "#" move-back-same-word
   "n" repeat-search+
   "N" repeat-search-
   "}" paragraph-forward
   "{" paragraph-backward
   "%" move-to-matched-braces
   "<c+u>" #(cursor-move-viewport %1 -0.5) 
   "<c+d>" #(cursor-move-viewport %1 0.5)})

(defn init-visual-mode-keymap[motion-keymap]
  (merge 
    motion-keymap 
    {"z" {"z" cursor-center-viewport}
     "y" yank-visual
     :enter (fn[buf keycode] 
              (-> buf
                  set-visual-mode
                  (assoc-in [:context :lastbuf] buf)))
     :leave (fn[buf keycode] (set-normal-mode buf))
     :continue #(not (or (= "d" %2) (= "c" %2) (= "<esc>" %2) (= "v" %2) (= "y" %2)))
     :after (fn[buf keycode]
              (-> buf
                  (visual-mode-select keycode)
                  (update-x-if-not-jk (buf :lastbuf) keycode)))
     "d" delete-range
     "c" change-range
     "o" swap-visual-start-end}))

(defn init-insert-mode-keymap[]
  {;"<c+o>" normal-mode-keymap 
   "<c+n>" #(autocompl-move % inc)
   "<c+p>" #(autocompl-move % dec)
   "<c+r>" {"<esc>" #(assoc % :keys [])
            :else put-from-register }
   :else insert-mode-default 
   :enter set-insert-mode
   :continue #(not (= "<esc>" %2))
   :leave (fn[buf keycode]
            (-> buf
                char-backward
                update-x
                set-normal-mode
                save-undo))})

(defn init-normal-mode-keymap[motion-keymap insert-mode-keymap visual-mode-keymap ex-mode-keymap]
  (merge 
    motion-keymap
    {"w" word-forward
     "W" WORD-forward
     "i" insert-mode-keymap
     "a" (merge
           insert-mode-keymap
           {:enter set-insert-append})
     "A" (merge
           insert-mode-keymap
           {:enter set-insert-line-end})
     "I" (merge
           insert-mode-keymap
           {:enter set-insert-line-start})
     "s" (merge
           insert-mode-keymap
           {:enter set-insert-remove-char})
     "O" (merge
           insert-mode-keymap
           {:enter set-insert-new-line-before})
     "o" (merge
           insert-mode-keymap
           {:enter set-insert-new-line})
     "." dot-repeat
     "=" (merge 
           motion-keymap
           {"=" #(-> % 
                     (update-in [:context] dissoc :lastbuf)
                     buf-indent-current-line
                     save-undo)
            :before save-lastbuf
            :after indent-motion})
     ":" (merge
           ex-mode-keymap
           {"<cr>" execute
            :enter (fn[buf keycode] (set-ex-mode buf))
            :leave (fn[buf keycode] (set-normal-mode buf))})
     "r" {"<esc>" identity
          :else replace-char-keycode}
     "u" undo
     "<c+r>" redo
     "<c+o>" #(move-to-jumplist % jump-prev)
     "<c+i>" #(move-to-jumplist % jump-next)
     "<c+g>" buf-pos-info
     "<esc>" set-normal-mode
     "v" visual-mode-keymap
     "z" {"z" cursor-center-viewport }
     "d" (merge 
           motion-keymap
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
     "J" buf-join-line
     "c" (merge
           motion-keymap 
           {:before save-lastbuf
            :after change-motion})
     "y" (merge
           motion-keymap
           {:before save-lastbuf
            :after yank-motion})
     "\"" {:else start-register}
     :before (fn [buf keycode]
               (-> buf
                   (assoc-in [:context :lastbuf] buf)
                   (assoc-in [:context :register] "\"")))
     :after normal-mode-after}))

(defn init-ex-mode-keymap[]
  {:continue #(not (or (= "<esc>" %2) (= "<cr>" %2) (empty? (:ex %1))))
   :leave (fn[buf keycode]
            (if (and (= "<esc>" keycode) (= \/ (-> buf :ex first)))
              (-> buf :context :lastbuf (assoc :ex ""))
              (assoc buf :ex "")))
   :else ex-mode-default})

(defn init-keymap-tree
  []
  (let [ex-mode-keymap (init-ex-mode-keymap)
        insert-mode-keymap (init-insert-mode-keymap)
        motion-keymap (init-motion-keymap ex-mode-keymap)
        visual-mode-keymap (init-visual-mode-keymap motion-keymap)
        normal-mode-keymap (init-normal-mode-keymap motion-keymap insert-mode-keymap visual-mode-keymap ex-mode-keymap)]
    (reset! root-keymap normal-mode-keymap)))

(defn- buf-bound-scroll-top
  "Change scroll top make cursor inside viewport"
  [buf]
  (let [st (-> buf :scroll-top)]
    (assoc buf :scroll-top 
           (let [y (buf :y)
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
