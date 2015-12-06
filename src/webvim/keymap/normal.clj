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
        webvim.utils
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
  (if (= "d" keycode)
    (-> buf
        setup-range-line
        (delete-range false true)
        line-start)
    (-> buf
        setup-range
        (delete-range (inclusive? keycode) false))))

(defn- yank[buf keycode]
  (if (= "y" keycode)
    (-> buf
        setup-range-line
        (yank-range false true))
    (-> buf
        setup-range
        (yank-range (inclusive? keycode) false))))

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
  (if (re-test #"[0-9a-zA-Z/*#%.:+=\-]" keycode)
    (assoc-in buf [:context :register] keycode)
    buf))

(defn- normal-mode-fix-pos
    "prevent cursor on top of EOL in normal mode"
    [buf]
    (let [ch (char-at (buf :str) (buf :pos))]
      (if (= (or ch \newline) \newline)
        (char- buf) buf)))

(defn- dot-repeat[buf]
  (let [keycodes (-> buf
                     (get-register ".")
                     :keys)]
    (println "keycodes:" keycodes)
    (if (empty? keycodes)
      buf
      ;Remove "." from :root-keymap prevent recursively execute dot-repeat which will cause stackoverflow
      ;Remove :before and :after because there are called in outside already
      (replay-keys buf
                   keycodes (-> buf :root-keymap
                                (dissoc "."))))))

;(def ^:private not-repeat-keys #{".", "u", "<c+r>", "p", "P", ":"})
(defn- replayable?[keycode]
  (not (contains? #{"." "u" "<c+r>" "p" "P" ":"} keycode)))

(defn- normal-mode-after[buf keycode]
  (let [lastbuf (-> buf :context :lastbuf)]
    (if-not (nil? (motions-push-jumps (string/join (buf :keys))))
      (jump-push lastbuf))
    ;(println "normal-mode-after, recording-keys" (-> buf :macro :recording-keys))
    ;if nothing changed there is no need to overwrite "." register
    ;so keys like i<esc> won't affect, this also exclude all motions.
    (let [keyvec (-> buf :macro :recording-keys)]
      (if (and (not (= (:str lastbuf) (:str buf)))
               (replayable? keycode)
               ;" is just register prefix, the actual keycode is the 3rd one
               (if (= keycode "\"")
                 (replayable? (nth keyvec 2))
                 true))
        (put-register! buf "." {:keys keyvec :str (string/join keyvec)})))
    (let [newbuf (normal-mode-fix-pos buf)]
      (-> newbuf
          set-normal-mode
          save-undo
          (update-x-if-not-jk keycode)
          ;alwasy clear :recording-keys
          (assoc-in [:macro :recording-keys] [])
          (update-in [:context] dissoc :range)
          (buf-update-highlight-brace-pair (newbuf :pos))))))

(defn- move-to-jumplist
  [buf fndir]
  (loop [pos (fndir buf)]  ;TODO: filter lazy seq instead of loop
    (if (nil? pos)
      buf ;newest or oldest
      (let [newb @(@buffer-list (pos :id))]
        (if (nil? newb)
          ;buffer has been deleted, ignore
          (recur (fndir buf))
          ;pos is avaliable
          (if (< (pos :pos) (count (newb :str)))
            (let [id (buf :id)
                  newid (pos :id)
                  newpos (pos :pos)]
              (if (= newid id)
                ;update pos inside current buffer
                (buf-set-pos buf newpos)
                (let []
                  (change-active-buffer id newid)
                  ;(swap! buffer-list update-in [newid] #(buf-set-pos % newpos))
                  (assoc buf :nextid newid))))
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
      (delete-range false false)))

(defn- change-to-line-end[buf]
  (-> buf
      setup-range-line-end
      (change-range false false)))

(defn- delete-and-insert[keymap insert-mode-keymap]
  (tree-map
    (fn[ks f]
      (if
        (let [s (clojure.string/join ks)]
          (or (and
                (contains? #{:else :before :after :enter :leave :continue "<esc>"} (first ks))
                (not (contains? #{":elset" ":elseT" ":elsef" ":elseF"} s)))
              (contains? #{"<bs>/"} s)))
        f
        (assoc
          insert-mode-keymap
          :enter
          (let [f (if (= (first ks) :else) f (fn[buf keycode] (f buf)))]
            (fn[buf keycode]
              (println "bufkeycode:" keycode)
              (-> buf
                  (f keycode)
                  setup-range
                  (change-range (inclusive? keycode) false))))))) keymap))

(defn init-normal-mode-keymap[motion-keymap insert-mode-keymap visual-mode-keymap visual-line-mode-keymap ex-mode-keymap pair-keymap]
  (let [enter-insert (insert-mode-keymap :enter)]
    (deep-merge
      motion-keymap
      {"i" insert-mode-keymap
       "a" (start-insert-mode char+ insert-mode-keymap)
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
       "g" {"v" (assoc
                  visual-mode-keymap
                  :enter
                  (fn[buf keycode]
                    (let [visual (buf :last-visual)]
                      (-> buf
                          ((visual-mode-keymap :enter) keycode)
                          (assoc :visual visual)
                          (buf-set-pos (-> visual :ranges first first)))))) }
       "v" visual-mode-keymap
       "V" visual-line-mode-keymap
       "z" {"z" cursor-center-viewport }
       "d" (merge
             motion-keymap
             pair-keymap
             {"d" identity
              :after delete})
       "c" (merge
             (delete-and-insert motion-keymap insert-mode-keymap)
             (delete-and-insert pair-keymap insert-mode-keymap)
             {"c" identity})
       "y" (merge
             motion-keymap
             pair-keymap
             {"y" identity
              :after yank})
       "=" (merge
             motion-keymap
             pair-keymap
             {"=" identity
              :after indent})
       "D" delete-to-line-end
       "C" change-to-line-end
       "Y" #(yank % "y")
       "x" delete-char
       "p" #(put-from-register-append % (-> % :context :register))
       "P" #(put-from-register % (-> % :context :register))
       "J" join-line
       "\"" {"<esc>" identity
             :else start-register}
       :before (fn [buf keycode]
                 (-> buf
                     (assoc-in [:context :lastbuf] buf)
                     (assoc-in [:context :range] nil)
                     (assoc-in [:context :register] "\"")))
       :after normal-mode-after})))

