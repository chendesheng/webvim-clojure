(ns webvim.keymap.normal
  (:require [clojure.string :as string])
  (:use clojure.pprint
        webvim.keymap.action
        webvim.keymap.macro
        webvim.keymap.motion
        webvim.keymap.insert
        webvim.keymap.addsub
        webvim.keymap.ex
        webvim.keymap.objects
        webvim.keymap.visual
        webvim.core.buffer
        webvim.core.rope
        webvim.core.line
        webvim.core.pos
        webvim.core.register
        webvim.indent
        webvim.core.utils
        webvim.jumplist))

(defn- delete-char [buf]
  (let [pos (buf :pos)
        [a b] [pos (inc pos)]]
    (buf-yank buf a b false true)
    (buf-delete buf a b)))

(defn- delete-line [buf]
  (let [pos (buf :pos)
        r (buf :str)
        [a b] (pos-line r pos)]
    (-> buf
        (buf-yank a b false true)
        (buf-replace a (dec b) "")
        buf-indent-current-line)))

(defn- insert-new-line [buf]
  (buf-indent-current-line
    (let [pos (buf :pos)
          r (buf :str)
          b (pos-line-last r pos)]
      (-> buf
          (buf-insert b <br>)
          (buf-set-pos b)))))

(defn- insert-new-line-before [buf]
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
  [buf keycode]
  (let [pos (buf :pos)
        r (buf :str)
        [a b] (pos-re+ r pos #"\r?\n.*?(?=(\r|\n|\S))")]
    (if (nil? a) buf
        (let [sep (if (or (>= b (count r))
                          (= (char-at r b) \))) "" " ")]
          (-> buf
              (buf-replace a b sep)
              (buf-set-pos a))))))

(defn- pos-info [buf]
  (let [{y :y
         x :x
         linescnt :linescnt} buf
        percent (-> y inc (* 100) (/ linescnt) int)]
    (assoc buf :message (format "\"%s\" line %d of %d --%d%%-- col %d" 
                                (printable-filepath buf)
                                (inc y) linescnt percent (inc x)))))

(defn- inclusive? [keycode]
  (println "keycode:" keycode)
  (let [m {"h" false "l" false "w" false "W" false "e" true
           "E" true "b" false "B" false "f" true "F" false
           "t" true "T" false "/" false "$" false "a" true
           "i" true}]
    (if (contains? m keycode)
      (m keycode)
      true)))

;setup range prefix for delete/change/yank etc.
(defn- setup-range [buf]
  (println "setup-range:" (-> buf :context :range))
  (if (-> buf :context :range nil?)
    (let [pos (buf :pos)
          lastbuf (-> buf :context :lastbuf)
          lastpos (lastbuf :pos)]
      (-> buf
          ;Restore positoin to lastbuf so that changes happen next can record correct start position. This will make cursor position in right place after undo/redo.
          (merge (select-keys lastbuf [:pos :x :y]))
          (assoc-in [:context :range] [lastpos pos]))) buf))

(defn- setup-range-line [buf]
  (assoc-in buf [:context :range] (pos-line (buf :str) (buf :pos))))

(defn- setup-range-line-end [buf]
  (let [a (buf :pos)
        b (pos-line-end (buf :str) a)]
    (assoc-in buf [:context :range] [a b])))

(defn- delete [buf keycode]
  (if (contains? #{"d" "j" "k"} keycode)
    (-> buf
        setup-range-line
        (delete-range false true)
        line-start)
    (-> buf
        setup-range
        (delete-range (inclusive? keycode) false))))

(defn- yank [buf keycode]
  (if (contains? #{"y" "j" "k" "Y"} keycode)
    (-> buf
        setup-range-line
        (yank-range false true))
    (-> buf
        setup-range
        (yank-range (inclusive? keycode) false))))

(defmethod replace-char-keycode :no-visual [buf keycode]
  (let [ch (keycode-to-char keycode)
        pos (buf :pos)]
    (cond
      (= ch "\n")
      (-> buf
          (buf-replace pos (inc pos) ch)
          (buf-set-pos (inc pos))
          buf-indent-current-line)
      (= (count ch) 1)
      (-> buf
          (buf-replace pos (inc pos) ch)
          (buf-set-pos pos))
      :else buf)))

(defn- normal-mode-after [buf keycode]
  (let [insert-mode? (= (buf :mode) insert-mode)
        lastbuf (-> buf :context :lastbuf)
        save-undo (if insert-mode? identity save-undo)]
    (if-not (nil? (motions-push-jumps (string/join (buf :keys))))
      (jump-push lastbuf))
    (let [newbuf (if insert-mode? buf
                     (normal-mode-fix-pos buf))]
      (-> newbuf
          (update-x-if-not-jk keycode)
          (update-in [:context] dissoc :range)
          save-undo))))

(defn- start-insert-mode-with-keycode [fnmotion fnedit]
  (fn [buf keycode]
    (-> buf 
        (fnmotion keycode)
        set-insert-mode
        (fnedit keycode))))

(defn- start-ex-mode [buf keycode]
  (let [enter (or (-> buf :ex-mode-keymap :enter) nop)]
    (-> buf
        (enter ":")
        (assoc 
          :keymap (buf :ex-mode-keymap)
          :mode ex-mode))))

(defn- delete-to-line-end [buf keycode]
  (-> buf
      setup-range-line-end
      (delete-range false false)))

(defn- change-to-line-end [buf]
  (-> buf
      setup-range-line-end
      (change-range false false)))

(defn- change-by-motion [buf keycode]
  (-> buf 
      setup-range
      (change-range (inclusive? keycode) false)))

(defn- path-under-cursor [buf]
  (let [r (buf :str)
        pos (buf :pos)
        filename-black-list "\\s:?%*|\"'<>"
        re-end (re-pattern (str "(?m)[^" filename-black-list "](:\\d+)?(?=[" filename-black-list "]|$)"))
        re-start (re-pattern (str "(?m)(?<=[" filename-black-list "]|^)[^" filename-black-list "]"))
        [_ end] (pos-re+ r pos re-end)
        [start _] (pos-re- r pos re-start)
        driver (let [driver (subr r (- start 2) start)]
                 (if (re-test #"[a-zA-Z]:" driver) driver ""))
        [[_ uri _ linenum]] (re-seq #"(([a-zA-Z]:)?[^:]+)(:\d+)?" (str driver (subr r start end)))]
    [uri linenum]))

(defn goto-file [buf]
  (let [[uri linenum] (path-under-cursor buf)]
    (if (nil? linenum)
      (edit-file buf uri false)
      (edit-file buf uri (parse-int linenum) false))))

(defn- dont-cross-line [f]
  (fn [buf keycode]
    (let [newbuf (f buf keycode)
          newpos (min (newbuf :pos) 
                      (pos-line-end (buf :str) (buf :pos)))]
      (buf-set-pos newbuf newpos))))

(defn- operator [f]
  (fn [buf keycode]
    (let [buf (setup-range buf)
          rg (range-prefix buf (inclusive? keycode))]
      (f buf rg))))

(defn- move-to-jumplist
  [fndir]
  (fn [buf keycode]
    (loop [pos (fndir buf)]  ;TODO: filter lazy seq instead of loop
      (if (nil? pos)
        buf ;newest or oldest
        (let [anewbuf (@buffer-list (pos :id))]
          (if (nil? anewbuf)
            ;buffer has been deleted, ignore
            (recur (fndir buf))
            ;pos is avaliable
            (if (< (pos :pos) (count (@anewbuf :str)))
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
              (recur (fndir buf)))))))))

(defn init-normal-mode-keymap [buf]
  (let [motion-keymap (init-motion-keymap)
        visual-mode-keymap (init-visual-mode-keymap motion-keymap buf)
        pair-keymap (init-pair-keymap)
        motion-keymap-fix-w (-> motion-keymap
                                (wrap-key "w" (fn [handler] (dont-cross-line handler)))
                                (wrap-key "W" (fn [handler] (dont-cross-line handler))))
        motion-keymap-fix-cw (-> motion-keymap
                                 ;vim's "cw" is identical to "ce", but "dw"/"yw" is not equal to "de"/"ye"
                                 (assoc "w" (dont-cross-line cw-move))
                                 (assoc "W" (dont-cross-line cW-move)))]
    (deep-merge
      motion-keymap
      {"i" (start-insert-mode identity identity)
       "a" (start-insert-mode char+ identity)
       "A" (start-insert-mode line-end identity)
       "I" (start-insert-mode line-start identity)
       "s" (start-insert-mode identity delete-char)
       "S" (start-insert-mode identity delete-line)
       "o" (start-insert-mode identity insert-new-line)
       "O" (start-insert-mode identity insert-new-line-before)
       ":" start-ex-mode
       "r" {"<esc>" nop
            :else replace-char-keycode}
       "u" (wrap-keycode undo)
       "<c-r>" (wrap-keycode redo)
       "<c-o>" (move-to-jumplist jump-prev)
       "<c-i>" (move-to-jumplist jump-next)
       "<c-g>" (wrap-keycode pos-info)
       "<esc>" (wrap-keycode set-normal-mode)
       "<f1>" (wrap-keycode #(goto-buf % (output-panel false)))
       "~" (merge
             motion-keymap-fix-w
             pair-keymap
             {:after (operator (change-case swap-case))})
       "g" {"v" (assoc
                  visual-mode-keymap
                  :enter
                  (fn [buf keycode]
                    (let [visual (buf :last-visual)]
                      (-> buf
                          (set-visual-mode visual)
                          (buf-set-pos (-> visual :range first))))))
            "f" (wrap-keycode goto-file)
            "u" (merge
                  motion-keymap-fix-w
                  pair-keymap
                  {:after (operator (change-case clojure.string/lower-case))})
            "U" (merge
                  motion-keymap-fix-w
                  pair-keymap
                  {:after (operator (change-case clojure.string/upper-case))})}
       "v" visual-mode-keymap
       "V" visual-mode-keymap
       "<c-v>" visual-mode-keymap
       "z" {"z" (wrap-keycode cursor-center-viewport)}
       "d" (merge
             motion-keymap-fix-w
             pair-keymap
             {"d" nop
              :after delete})
       "c" (merge
             motion-keymap-fix-cw
             pair-keymap
             {"c" (start-insert-mode identity delete-line)
              :after (fn [buf keycode]
                       (if (or (= keycode "c")
                               (and
                                 (= (-> buf :context :lastbuf :pos) (buf :pos))
                                 (-> buf :context :range empty?)))
                         buf
                         ((start-insert-mode-with-keycode nop change-by-motion) buf keycode)))})
       "y" (merge
             motion-keymap-fix-w
             pair-keymap
             {"y" nop
              :after yank})
       "=" (merge
             motion-keymap-fix-w
             pair-keymap
             {"=" nop
              :after (fn [buf keycode]
                       (if (contains? #{"=" "j" "k"} keycode)
                         (buf-indent-current-line buf)
                         (-> buf
                             setup-range
                             (indent-range true))))})
       "D" delete-to-line-end
       "C" (start-insert-mode identity change-to-line-end)
       "Y" yank
       "x" (wrap-keycode delete-char)
       "p" (fn [buf keycode]
             (let [append? (if (-> buf :context :register registers-get :linewise?)
                             true 
                             (not= (char-at (buf :str) (buf :pos)) \newline))]
               ;(println "append?" append?)
               (put-from-register buf (-> buf :context :register) append?)))
       "P" (wrap-keycode #(put-from-register % (-> % :context :register) false))
       "J" join-line
       "<c-s-6>" (fn [buf keycode]
                   (let [reg (registers-get "#")]
                     (if (nil? reg)
                       (assoc buf :message "No alternative file")
                       (goto-buf buf (get-buffer-from-reg reg)))))
       ">" (merge
             motion-keymap-fix-w
             pair-keymap
             {:after (operator indent-more)})
       "<" (merge
             motion-keymap-fix-w
             pair-keymap
             {:after (operator indent-less)})
       :continue (fn [buf keycode]
                   (= (buf :mode) normal-mode))
       :before (fn [buf keycode]
                 (-> buf
                     (assoc-in [:context :lastbuf] buf)
                     (assoc-in [:context :range] nil)))
       :after normal-mode-after})))

