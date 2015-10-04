(ns webvim.keymap.normal
  (:use webvim.keymap.action
        webvim.keymap.macro
        webvim.keymap.motion
        webvim.keymap.insert
        webvim.keymap.ex
        webvim.core.buffer
        webvim.core.rope
        webvim.core.line
        webvim.core.pos
        webvim.core.register
        webvim.core.serve
        webvim.indent
        webvim.utils
        webvim.jumplist
        webvim.autocompl))

(defn- delete-char[buf]
  (let [pos (buf :pos)]
    (registers-put (buf :registers) (-> buf :context :register) (buf-copy-range buf pos pos true))
    (buf-delete-offset buf 1)))

(defn- cut-char[b]
    (-> b 
        delete-char
        save-undo))

(defn- change-to-line-end[buf]
  (-> buf
      (buf-delete (buf :pos) (-> buf current-line last dec))
      (serve-keymap (-> buf :root-keymap (get "i")) "c")))

(defn- insert-new-line[buf]
  (-> buf 
      insert-line-after
      buf-indent-current-line))

(defn- insert-new-line-before[buf]
  (-> buf 
      insert-line-before
      buf-indent-current-line))

(defn- buf-join-line
  "join current line and next line"
  [buf]
  (let [pos (buf :pos)
        r (buf :str)
        [a b] (pos-re+ r pos #"\n.+?(?=(\n|\S))")]
    (if (nil? a) buf
      (-> buf
          (buf-replace a b " ")
          save-undo))))

(defn- buf-pos-info[b]
  (let [{nm :name 
         path :filepath
         y :y} b]
    (assoc b :message (str "\"" (or path nm) "\" line " (inc y)))))

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

(defn- inclusive? [keycode]
  (let [res (map-key-inclusive keycode)]
    (if (nil? res)
      true
      res)))

(defn- buf-copy-range-lastbuf[b cur inclusive]
  (registers-put (:registers b) (-> b :context :register) (buf-copy-range b cur (b :pos) inclusive))
  b)

(defn- delete-motion[b keycode]
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

(defn- yank-motion[b keycode]
  (let [lastbuf (-> b :context :lastbuf)
        lastpos (:pos lastbuf)
        pos (b :pos)]
    (if (or (nil? lastbuf) (= pos lastpos))
      b
      (let [inclusive (inclusive? keycode)]
        (buf-copy-range-lastbuf b lastpos inclusive)
        lastbuf))))

(defn- indent-motion[b keycode]
  (let [lastbuf (-> b :context :lastbuf)
        lastpos (:pos lastbuf)
        pos (b :pos)]
    (if (or (nil? lastbuf) (= pos lastpos))
      b
      (-> b 
          (buf-indent-lines (sort2 pos lastpos))
          save-undo))))

(defn- replace-char-keycode[buf keycode]
  (let [ch (keycode-to-char buf)]
    (if (= (count ch) 1)
      (let [enter-indent (if (= ch "\n") 
                           #(buf-indent-current-line %)
                           identity)]
        (-> buf 
            (buf-replace-char ch)
            enter-indent
            save-undo))
      buf)))

(defn- start-register[buf keycode]
  (let [m (re-find #"[0-9a-zA-Z/*#%.:+=\-]" keycode)]
    (if (not (nil? m))
      (-> buf 
          (assoc-in [:context :register] keycode)
          (serve-keymap (-> buf :root-keymap (get "i")) keycode)))))

(defn- change-motion[buf keycode]
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

(defn- normal-mode-fix-pos
    "prevent cursor on top of EOL in normal mode"
    [buf]
    (let [ch (char-at (buf :str) (buf :pos))]
      (if (= (or ch \newline) \newline)
        (char-backward buf) buf)))

(defn- dot-repeat[buf]
  (let [keycodes (registers-get (:registers buf) ".")]
    (if (empty? keycodes)
      buf
      ;remove "." from @root-keymap prevent recursive, probably useless
      (replay-keys buf keycodes (-> buf :root-keymap (dissoc "."))))))

(defn- normal-mode-after[buf keycode]
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

(defn- move-to-jumplist
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

(defn- start-insert-mode[fnmotion insert-mode-keymap]
  (merge
    insert-mode-keymap
    {:enter (fn[buf keycode]
              (-> buf
                  fnmotion
                  ((insert-mode-keymap :enter) keycode)))}))

(defn- start-insert-mode-insert[insert-mode-keymap fnedit]
  (merge
    insert-mode-keymap
    {:enter (fn[buf keycode]
              (-> buf
                  ((insert-mode-keymap :enter) keycode)
                  fnedit))}))

(defn init-normal-mode-keymap[motion-keymap insert-mode-keymap visual-mode-keymap ex-mode-keymap]
  (let [enter-insert (insert-mode-keymap :enter)]
  (merge 
    motion-keymap
    {"i" insert-mode-keymap
     "a" (start-insert-mode char-forward insert-mode-keymap)
     "A" (start-insert-mode line-end insert-mode-keymap)
     "I" (start-insert-mode line-start insert-mode-keymap)
     "s" (start-insert-mode-insert insert-mode-keymap delete-char)
     "o" (start-insert-mode-insert insert-mode-keymap insert-new-line)
     "O" (start-insert-mode-insert insert-mode-keymap insert-new-line-before)
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
           {:leave (fn[buf keycode] (set-normal-mode buf))})
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
     :after normal-mode-after})))

