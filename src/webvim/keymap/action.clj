(ns webvim.keymap.action
  (:use webvim.core.buffer
        webvim.core.rope
        webvim.core.line
        webvim.core.pos
        webvim.core.register
        webvim.core.serve
        webvim.utils))

(defonce normal-mode 0)
(defonce insert-mode 1)
(defonce visual-mode 2)
(defonce ex-mode 3)

(defn pos-match-brace
  "return matched brace position, nil if not find"
  [r pos]
  (let [brace (char-at r pos)
        m (all-braces brace)
        left? (contains? left-braces brace)
        re (re-pattern (str  (quote-pattern brace) "|" (quote-pattern m)))]
    (if (nil? m) nil
      (let [inc-cnt? (if left? 
                       #(contains? left-braces %)
                       #(contains? right-braces %))
            braces (if left?
                     (pos-re-seq+ r pos re)
                     (pos-re-seq- r pos re))
            mpos (reduce 
                   (fn[cnt [a _]]
                     (let [ch (char-at r a)
                           newcnt (if (inc-cnt? ch)
                                    (inc cnt)
                                    (dec cnt))]
                       (if (zero? newcnt)
                         (reduced [a])
                         newcnt))) 0 braces)]
        (if (vector? mpos) (first mpos) nil)))))

(defn buf-update-highlight-brace-pair[buf pos]
  (let [mpos (pos-match-brace (buf :str) pos)]
    ;(println pos mpos)
    (if (nil? mpos)
      (dissoc buf :braces)
      (assoc buf :braces [pos mpos]))))

(defn- add-highlight[buf rg]
  (let [highlights (buf :highlights)]
    (if (empty? (filter (fn[[a b]]
                          (and (= a (rg 0)) (= b (rg 1)))) highlights))
      (update-in buf [:highlights] conj rg) buf)))

(defn re-forward-highlight[buf re]
  (let [pos (buf :pos)
        r (buf :str)
        rg (or 
             (pos-re+ r (inc pos) re)
             (pos-re+ r 0 re))] ;TODO: use region reduce duplicate work
    (if (nil? rg) buf
      (let [[a b] rg]
        (-> buf
            (buf-set-pos a)
            (add-highlight [a (dec b)]))))))

(defn re-backward-highlight[buf re]
  (let [pos (buf :pos)
        r (buf :str)
        rg (or 
             (pos-re- r (dec pos) re)
             (pos-re- r (-> r count dec) re))]
    (if (nil? rg) buf
      (let [[a b] rg]
        (-> buf
            (buf-set-pos a)
            (add-highlight [a (dec b)]))))))

(defn set-insert-mode[buf keycode]
  ;(println "set-insert-mode")
  (merge buf {:ex "" 
              :mode insert-mode 
              :message nil 
              :keys nil}))

(defn set-normal-mode[buf]
  ;(println "set-normal-mode:")
  (merge buf {:ex "" 
            :mode normal-mode 
            :keys nil
            :visual {:type 0 :ranges nil}
            :autocompl {:suggestions nil 
                        :suggestions-index 0}}))

(defn buf-delete
  ([buf a b]
   (buf-replace buf a b ""))
  ([buf b]
   (let [pos (buf :pos)
         [a b] (sort2 pos b)]
     (buf-delete buf a b))))

(defn change-active-buffer[id]
  (registers-put registers "#" @active-buffer-id)
  (reset! active-buffer-id id)
  (registers-put registers "%" id))

(defn delete-inclusive
  [buf a b]
  (let [[a b] (sort2 a b)]
    (-> buf
        (buf-delete a (inc b))
        (buf-set-pos a))))

(defn change-range[buf]
  (let [[a b] (-> buf :visual :ranges first)]
    (-> buf
        (delete-inclusive a b)
        (set-insert-mode "c")
        (serve-keymap (-> buf :root-keymap (get "i")) "c"))))

(defn update-x[buf]
  (let [pos (buf :pos)]
    (assoc buf :x (- pos (pos-line-first (buf :str) pos)))))

(defn update-x-if-not-jk
  "update :x unless it is up down motion"
  [buf lastbuf keycode]
  (if-not (or (= (:pos lastbuf) (:pos buf)) 
              (contains? #{"j" "k"} keycode))
    (update-x buf) buf))

;one server only serve one window at one time
(defonce window (atom{:viewport {:w 0 :h 0}}))

(defn round-to-zero
  "(round-to-zero -9.1) = -9; (round-to-zero 9.1) = 9"
  [i]
  (if (> i 0)
    (int i)
    (- (int (- i)))))

(defn negzero[n]
  (if (neg? n) 0 n))

(defn cursor-move-viewport
  "Jump cursor by viewport height, deps to window's :viewport"
  [buf factor]
  (let [d (round-to-zero (* (:h (:viewport @window)) factor))
        scroll-top (buf :scroll-top)
        h (-> @window :viewport :h)
        row (-> buf :y)
        vrow (- row scroll-top)
        newrow (bound-range (+ row d) 0 (buf :linescnt))
        newst (-> newrow (- vrow) negzero)]
    (-> buf
        (assoc :scroll-top newst)
        (lines-row newrow))))

(defn cursor-center-viewport[buf]
  (assoc buf :scroll-top 
            (-> buf :y
                (- (int (/ (-> @window :viewport :h) 2))))))


(defn buf-copy-range[buf a b inclusive]
  (let [[a b] (sort2 a b)]
    (str (subr (buf :str) a (if inclusive (inc b) b)))))

(defn save-lastbuf[buf keycode]
  (-> buf (assoc-in [:context :lastbuf] buf)))

(defn buf-replace-char [buf ch]
  (let [pos (buf :pos)]
    (buf-replace buf pos (inc pos) ch)))

(defn buf-insert
  ([buf pos txt]
   (buf-replace buf pos pos txt))
  ([buf txt]
   (buf-insert buf (buf :pos) txt)))

(defn buf-delete-range
  "delete range and set pos to end of deleted"
  [buf rg]
  (-> buf
      (buf-delete (first rg) (second rg))
      (buf-set-pos (first rg))))

(defn delete-line[buf]
  (let [pos (buf :pos)
        [a b] (current-line buf)]
    (registers-put (:registers buf) (-> buf :context :register) (buf-copy-range buf a b false))
    (-> buf
        (update-in [:context] dissoc :lastbuf) ;remove :lastbuf prevent delete-motion take over.
        (buf-delete-range (current-line buf))
        line-start
        save-undo)))

(defn delete-range[buf]
  (let [[a b] (-> buf :visual :ranges first)]
    (-> buf
        (delete-inclusive a b)
        save-undo)))

(defn buf-delete-offset[buf offset] 
  (let [pos (buf :pos)
        newpos (+ pos offset)]
    (if (neg? newpos) buf
      (buf-delete buf newpos))))

(defn insert-line-after[buf]
  (let [pos (buf :pos)
        [_ b] (current-line buf)]
    (-> buf
        (buf-insert b <br>)
        (buf-set-pos b))))

(defn insert-line-before[buf]
  (let [pos (buf :pos)
        [a b] (current-line buf)]
    (if (zero? a)
      (-> buf
          (buf-insert 0 <br>)
          buf-start)
      (-> buf
          (buf-set-pos (- a 1))
          (buf-insert <br>)))))

(defn put-from-register[buf keycode]
  (let [txt (registers-get (:registers buf) keycode)]
    (if (string? txt)
      (-> buf
          (buf-insert txt)
          char-backward
          save-undo)
      buf)))

(defn put-from-register-append[buf keycode]
  (let [txt (registers-get (:registers buf) keycode)]
    (if (string? txt)
      (let [pos (buf :pos)]
        (-> buf
            (buf-insert (inc pos) txt)
            (buf-set-pos (+ pos (count txt)))
            save-undo))
      buf)))

