(ns webvim.keymap.normal
  (:require [clojure.string :as string])
  (:use clojure.pprint
        webvim.keymap.action
        webvim.keymap.macro
        webvim.keymap.motion
        webvim.keymap.insert
        webvim.keymap.ex
        webvim.core.buffer
        webvim.core.rope
        webvim.core.line
        webvim.core.pos
        webvim.core.register
        webvim.indent
        webvim.core.utils
        webvim.jumplist
        webvim.autocompl))

(defn- delete-char[buf]
  (let [pos (buf :pos)
        [a b] [pos (inc pos)]]
    (buf-yank buf a b false)
    (buf-delete buf a b)))

(defn- insert-new-line[buf]
  (buf-indent-current-line
    (let [pos (buf :pos)
          r (buf :str)
          b (pos-line-last r pos)]
      (-> buf
          (buf-insert b <br>)
          (buf-set-pos b)))))

(defn- insert-new-line-before[buf]
  (buf-indent-current-line
    (let [pos (buf :pos)
          r (buf :str)
          a (pos-line-first r pos)]
      (if (zero? a)
        (-> buf
            (buf-insert 0 <br>)
            buf-start)
        (-> buf
            (buf-set-pos (- a 1))
            (buf-insert <br>))))))

(defn- join-line
  "join current line and next line"
  [buf]
  (let [pos (buf :pos)
        r (buf :str)
        [a b] (pos-re+ r pos #"\r?\n.*?(?=(\r|\n|\S))")]
    (if (nil? a) buf
      (-> buf
          (buf-replace a b " ")
          (buf-set-pos a)))))

(defn- buf-pos-info[buf]
  (let [{y :y
         x :x
         linescnt :linescnt} buf
        percent (-> y inc (* 100) (/ linescnt) int)]
    (assoc buf :message (format "\"%s\" line %d of %d --%d%%-- col %d" 
                                (printable-filepath buf)
                                (inc y) linescnt percent (inc x)))))

(def ^:private map-key-inclusive
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
   "$" false
   "a" true
   "i" true})

(defn- inclusive? [keycode]
  (let [res (map-key-inclusive keycode)]
    (if (nil? res)
      true
      res)))

;setup range prefix for delete/change/yank etc.
(defn- setup-range[buf]
  (println (-> buf :context :range))
  (if (-> buf :context :range nil?)
    (let [pos (buf :pos)
          lastbuf (-> buf :context :lastbuf)
          lastpos (lastbuf :pos)]
      (-> buf
          ;Restore positoin to lastbuf so that changes happen next can record correct start position. This will make cursor position in right place after undo/redo.
          (merge (select-keys lastbuf [:pos :x :y]))
          (assoc-in [:context :range] [lastpos pos]))) buf))

(defn- setup-range-line[buf]
  (assoc-in buf [:context :range] (pos-line (buf :str) (buf :pos))))

(defn- setup-range-line-end[buf]
  (let [a (buf :pos)
        b (pos-line-end (buf :str) a)]
    (assoc-in buf [:context :range] [a b])))

(defn- delete[buf keycode]
  (if (contains? #{"d" "j" "k"} keycode)
    (-> buf
        setup-range-line
        (delete-range false true)
        line-start)
    (-> buf
        setup-range
        (delete-range (inclusive? keycode) false))))

(defn- yank[buf keycode]
  (if (contains? #{"y" "j" "k"} keycode)
    (-> buf
        setup-range-line
        (yank-range false true))
    (-> buf
        setup-range
        (yank-range (inclusive? keycode) false))))

(defn- indent[buf keycode]
  (if (contains? #{"=" "j" "k"} keycode)
    (buf-indent-current-line buf)
    (-> buf
        setup-range
        (indent-range true))))

(defn- replace-char-keycode[buf keycode]
  (let [ch (keycode-to-char keycode)]
    (if (= (count ch) 1)
      (let [enter-indent (if (= ch "\n")
                           #(buf-indent-current-line %)
                           identity)
            pos (buf :pos)]
        (-> buf
            (buf-replace pos (inc pos) ch)
            (buf-set-pos pos)
            enter-indent))
      buf)))

(defn- start-register[buf keycode]
  (if (re-test #"[0-9a-zA-Z/*#%.:+=\-]" keycode)
    (assoc-in buf [:context :register] keycode)
    buf))

(defn- dot-repeat[buf]
  (let [keycodes (-> buf
                     (get-register ".")
                     :keys)]
    (if (empty? keycodes)
      buf
      (replay-keys buf keycodes))))

(defn- replayable?[keycode]
  (not (contains? #{"." "u" "<c+r>" "p" "P" ":"} keycode)))

(defn- reset-context-register[buf keycode]
  (if (= keycode "\"") buf
    (assoc-in buf [:context :register] "\""))) ;reset

(defn- normal-mode-after[buf keycode]
  (let [lastbuf (-> buf :context :lastbuf)
        save-undo (if (= (buf :mode) insert-mode) identity save-undo)]
    (if-not (nil? (motions-push-jumps (string/join (buf :keys))))
      (jump-push lastbuf))
    (-> buf
        (reset-context-register keycode)
        (update-x-if-not-jk keycode)
        ;alwasy clear :recording-keys
        (assoc-in [:macro :recording-keys] [])
        (update-in [:context] dissoc :range)
        save-undo
        ;TODO make brace match async
        (buf-update-highlight-brace-pair (buf :pos)))))

(defn- start-insert-mode-with-keycode [keycode fnmotion fnedit]
  (fn[buf _]
    (-> buf 
        (fnmotion keycode)
        (set-insert-mode keycode)
        (assoc-in [:macro :keys] (buf :keys)) ;for dot repeat
        (fnedit keycode))))

(defn- start-ex-mode[buf]
  (-> buf
      (line-editor-enter ":")
      (assoc :mode ex-mode)))

(defn- delete-to-line-end[buf]
  (-> buf
      setup-range-line-end
      (delete-range false false)))

(defn- change-to-line-end[buf]
  (-> buf
      setup-range-line-end
      (change-range false false)))

(defn- change-by-motion[buf keycode]
  (-> buf 
      setup-range
      (change-range (inclusive? keycode) false)))

(defn- path-under-cursor[buf]
  (let [r (buf :str)
        pos (buf :pos)
        filename-black-list "\\s:?%*|\"'<>"
        re-end (re-pattern (str "(?m)[^" filename-black-list "](:\\d+)?(?=[" filename-black-list "]|$)"))
        re-start (re-pattern (str "(?m)(?<=[" filename-black-list "]|^)[^" filename-black-list "]"))
        [_ end] (pos-re+ r pos re-end)
        [start _] (pos-re- r pos re-start)
        [[_ uri linenum]] (re-seq #"([^:]+)(:\d+)?" (str (subr r start end)))]
    [uri linenum]))
        
(defn goto-file[buf]
  (let [[uri linenum] (path-under-cursor buf)
        newbuf (edit-file buf uri false)
        nextid (newbuf :nextid)]
    (if (nil? nextid) newbuf
      (let[anextbuf (@buffer-list nextid)]
        (send anextbuf (fn[buf row]
                         (if (<= row 0) buf
                           (move-to-line buf (dec row)))) (parse-int linenum))
        newbuf))))

(defn- dont-cross-line[f]
  (fn[buf]
    (let [newbuf (f buf)
          newpos (min (newbuf :pos) 
                      (pos-line-end (buf :str) (buf :pos)))]
      (buf-set-pos newbuf newpos))))

(defn init-normal-mode-keymap[motion-keymap insert-mode-keymap visual-mode-keymap visual-line-mode-keymap ex-mode-keymap pair-keymap]
  (let [enter-insert (insert-mode-keymap :enter)
        motion-keymap-fix-w (-> motion-keymap
                                (assoc "w" (dont-cross-line (motion-keymap "w")))
                                (assoc "W" (dont-cross-line (motion-keymap "W"))))]
    (deep-merge
      motion-keymap
      {"i" (start-insert-mode "i" identity identity)
       "a" (start-insert-mode "a" char+ identity)
       "A" (start-insert-mode "A" line-end identity)
       "I" (start-insert-mode "I" line-start identity)
       "s" (start-insert-mode "s" identity delete-char)
       "o" (start-insert-mode "o" identity insert-new-line)
       "O" (start-insert-mode "O" identity insert-new-line-before)
       "." dot-repeat
       ":" start-ex-mode
       "r" {"<esc>" identity
            :else replace-char-keycode}
       "u" undo
       "<c+r>" redo
       "<c+o>" #(move-to-jumplist % jump-prev)
       "<c+i>" #(move-to-jumplist % jump-next)
       "<c+g>" buf-pos-info
       "<esc>" set-normal-mode
       "<f1>" #(goto-buf % (output-buf false))
       "g" {"v" (assoc
                  visual-mode-keymap
                  :enter
                  (fn[buf keycode]
                    (let [visual (buf :last-visual)]
                      (-> buf
                          ((visual-mode-keymap :enter) keycode)
                          (assoc :visual visual)
                          (buf-set-pos (-> visual :ranges first first)))))) 
            "f" goto-file}
       "v" visual-mode-keymap
       "V" visual-line-mode-keymap
       "z" {"z" cursor-center-viewport }
       "d" (merge
             motion-keymap-fix-w
             pair-keymap
             {"d" identity
              :after delete})
       "c" (merge
             motion-keymap-fix-w
             pair-keymap
             {:leave (start-insert-mode-with-keycode "c" nop change-by-motion)
              "c" identity})
       "y" (merge
             motion-keymap-fix-w
             pair-keymap
             {"y" identity
              :after yank})
       "=" (merge
             motion-keymap-fix-w
             pair-keymap
             {"=" identity
              :after indent})
       "D" delete-to-line-end
       "C" (start-insert-mode "C" identity change-to-line-end)
       "Y" #(yank % "y")
       "x" delete-char
       "p" (fn[buf]
             (if (= (char-at (buf :str) (buf :pos)) \newline)
               (put-from-register buf (-> buf :context :register))
               (put-from-register-append buf (-> buf :context :register))))
       "P" #(put-from-register % (-> % :context :register))
       "J" join-line
       "\"" {"<esc>" identity
             :else start-register}
       "<c+s+6>" (fn[buf]
                   (println "switch to alternative")
                   (let [reg (@registers "#")]
                     (if (nil? reg)
                       (assoc buf :message "No alternative file")
                       (goto-buf buf (get-buffer-from-reg reg)))))
       :continue (fn[buf keycode]
                   (= (buf :mode) normal-mode))
       :before (fn [buf keycode]
                 (-> buf
                     (assoc-in [:context :lastbuf] buf)
                     (assoc-in [:context :range] nil)))
       :after normal-mode-after})))

