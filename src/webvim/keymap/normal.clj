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
  (let [pos (buf :pos)
        [a b] [pos (inc pos)]]
    (buf-yank buf a b)
    (buf-delete buf a b)))

(defn- insert-new-line[buf]
  (buf-indent-current-line
    (let [pos (buf :pos)
          [_ b] (current-line buf)]
      (-> buf
          (buf-insert b <br>)
          (buf-set-pos b)))))

(defn- insert-new-line-before[buf]
  (buf-indent-current-line 
    (let [pos (buf :pos)
          [a b] (current-line buf)]
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
        [a b] (pos-re+ r pos #"\n.+?(?=(\n|\S))")]
    (if (nil? a) buf
      (-> buf
          (buf-replace a b " ")))))

(defn- buf-pos-info[buf]
  (let [{nm :name 
         path :filepath
         y :y
         x :x
         linescnt :linescnt} buf
        percent (-> y inc (* 100) (/ linescnt) int)]
    (assoc buf :message (format "\"%s\" line %d of %d --%d%%-- col %d" (or path nm) (inc y) linescnt percent (inc x)))))

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

;setup range prefix for delete/change/yank etc.
(defn- setup-range[buf]
  (let [lastbuf (-> buf :context :lastbuf)]
    (assoc-in lastbuf [:context :range] [(lastbuf :pos) (buf :pos)])))

(defn- setup-range-line[buf]
  (let [pos (buf :pos)
        r (buf :str)
        a (pos-line-first r pos)
        b (pos-line-end r pos)] ;TODO: use current-line
    (assoc-in buf [:context :range] [a b])))

(defn- setup-range-line-end[buf]
  (let [a (buf :pos)
        b (pos-line-end (buf :str) a)]
    (assoc-in buf [:context :range] [a b])))

(defn- delete[buf keycode]
  (if (= "d" keycode)
    (-> buf
        setup-range-line 
        (delete-range (inclusive? keycode))
        line-start)
    (-> buf
        setup-range
        (delete-range (inclusive? keycode)))))

(defn- yank[buf keycode]
  (if (= "y" keycode)
    (-> buf
        setup-range-line 
        (yank-range (inclusive? keycode)))
    (-> buf
        setup-range
        (yank-range (inclusive? keycode)))))

(defn- change[buf keycode]
  (if (= "c" keycode)
    buf
    ;setup-range-line 
    ;(buf-delete (-> buf :context :range))
    ;buf-indent-current-line
    ;(set-insert-mode "c")
    ;(serve-keymap (-> buf :root-keymap (get "i")) "c"))
    (-> buf
        setup-range
        (change-range (inclusive? keycode)))))

(defn- indent[buf keycode]
  (if (= "=" keycode)
    (buf-indent-current-line buf)
    (-> buf
        setup-range
        (indent-range true))))

(defn- replace-char-keycode[buf keycode]
  (let [ch (keycode-to-char buf)]
    (if (= (count ch) 1)
      (let [enter-indent (if (= ch "\n") 
                           #(buf-indent-current-line %)
                           identity)
            pos (buf :pos)]
        (-> buf 
            (buf-replace buf pos (inc pos) ch)
            enter-indent))
      buf)))

(defn- start-register[buf keycode]
  (let [m (re-find #"[0-9a-zA-Z/*#%.:+=\-]" keycode)]
    (if (not (nil? m))
      (-> buf 
          (assoc-in [:context :register] keycode)
          (serve-keymap (-> buf :root-keymap (dissoc "\"")) keycode)))))

(defn- normal-mode-fix-pos
    "prevent cursor on top of EOL in normal mode"
    [buf]
    (let [ch (char-at (buf :str) (buf :pos))]
      (if (= (or ch \newline) \newline)
        (char-backward buf) buf)))

(defn- dot-repeat[buf]
  (let [keycodes (get-register buf ".")]
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
        save-undo
        normal-mode-fix-pos
        (update-x-if-not-jk lastbuf keycode)
        ;alwasy clear :recording-keys
        (assoc-in [:macro :recording-keys] [])
        (update-in [:context] dissoc :range) 
        (buf-update-highlight-brace-pair (buf :pos)))))

(defn- move-to-jumplist
  [buf fndir]
  (loop [pos (fndir buf)]
    (if (nil? pos)
      buf ;newest or oldest
      (let [newb (@buffer-list (pos :id))]
        (if (nil? newb)
          ;buffer has been deleted, ignore
          (recur (fndir buf)) 
          ;pos is avaliable
          (if (< (pos :pos) (count (newb :str)))
            (let [newid (newb :id)
                  newpos (pos :pos)]
              (if (= newid @active-buffer-id) 
                ;update pos inside current buffer
                (buf-set-pos buf newpos)
                (let []
                  (change-active-buffer newid)
                  (swap! buffer-list update-in [newid] #(buf-set-pos % newpos))
                  buf)))
            ;buffer has been modifed and cursor is no longer inside, ignore
            (recur (fndir buf))))))))

(defn- start-insert-mode[fnmotion insert-mode-keymap]
  (merge
    insert-mode-keymap
    {:enter (fn[buf keycode]
              (-> buf
                  fnmotion
                  ((insert-mode-keymap :enter) keycode)))}))

(defn- start-insert-mode-insert[fnedit insert-mode-keymap]
  (merge
    insert-mode-keymap
    {:enter (fn[buf keycode]
              (-> buf
                  ((insert-mode-keymap :enter) keycode)
                  fnedit))}))

(defn- delete-to-line-end[buf]
  (-> buf
      setup-range-line-end
      (delete-range false)))

(defn- change-to-line-end[buf]
  (-> buf 
      setup-range-line-end
      (change-range false)))

(defn init-normal-mode-keymap[motion-keymap insert-mode-keymap visual-mode-keymap ex-mode-keymap]
  (let [enter-insert (insert-mode-keymap :enter)]
  (merge 
    motion-keymap
    {"i" insert-mode-keymap
     "a" (start-insert-mode char-forward insert-mode-keymap)
     "A" (start-insert-mode line-end insert-mode-keymap)
     "I" (start-insert-mode line-start insert-mode-keymap)
     "s" (start-insert-mode-insert delete-char insert-mode-keymap)
     "o" (start-insert-mode-insert insert-new-line insert-mode-keymap)
     "O" (start-insert-mode-insert insert-new-line-before insert-mode-keymap)
     "." dot-repeat
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
           {"d" identity
            :after delete})
     "c" (merge
           motion-keymap 
           {"c" identity
            :after change})
     "y" (merge
           motion-keymap
           {"y" identity
            :after yank})
     "=" (merge 
           motion-keymap
           {"=" identity
            :after indent})
     "D" delete-to-line-end
     "C" change-to-line-end
     "Y" #(yank % "y")
     "x" delete-char
     "p" #(put-from-register-append % (-> % :context :register))
     "P" #(put-from-register % (-> % :context :register))
     "J" join-line
     "\"" {:else start-register}
     :before (fn [buf keycode]
               (-> buf
                   (assoc-in [:context :lastbuf] buf)
                   (assoc-in [:context :register] "\"")))
     :after normal-mode-after})))

