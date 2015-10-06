(ns webvim.keymap.action
  (:use webvim.core.buffer
        webvim.core.rope
        webvim.core.line
        webvim.core.pos
        webvim.core.register
        webvim.core.serve
        webvim.indent
        webvim.utils))

(defonce normal-mode 0)
(defonce insert-mode 1)
(defonce visual-mode 2)
(defonce ex-mode 3)

(defn get-register[buf c]
  (registers-get (buf :registers) c))

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

(defn buf-yank[buf [a b]]
  (let [s (buf-subr buf a b)]
    (registers-put (buf :registers) 
                   (-> buf :context :register)
                   s)
    (update-in buf [:context] dissoc :register)))

(defn change-active-buffer[id]
  (registers-put registers "#" @active-buffer-id)
  (reset! active-buffer-id id)
  (registers-put registers "%" id))

;collect range argument, TODO: add linewise
(defn range-prefix[buf inclusive?]
  (cond 
    (-> buf :mode (= visual-mode))
    (-> buf :visual :ranges (get 0) (make-range inclusive?))
    (-> buf :context :range nil? not) ;TODO: this looks like hack
    (-> buf :context :range (make-range inclusive?))
    (-> buf :context :lastpos nil? not)
    (make-range (-> buf :context :lastpos) (buf :pos) inclusive?)))

(defn change-range[buf inclusive?]
  (let [[a b] (range-prefix buf inclusive?)]
    (-> buf
        (buf-delete a b)
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

(defn cursor-center-viewport[buf]
  (assoc buf :scroll-top 
            (-> buf :y
                (- (int (/ (-> @window :viewport :h) 2))))))

(defn save-lastbuf[buf keycode]
  (-> buf (assoc-in [:context :lastbuf] buf)))

(defn save-lastpos[buf keycode]
  (-> buf (assoc-in [:context :lastpos] (buf :pos))))

(defn delete-range[buf inclusive?]
  (let [[a b] (range-prefix buf inclusive?)]
    (println "delete-range:" a b)
    (-> buf
        (buf-yank [a b])
        (buf-delete a b)
        (buf-set-pos a))))

(defn yank-range[buf inclusive?]
  (let [rg (range-prefix buf inclusive?)]
    (buf-yank buf rg)))

(defn indent-range[buf inclusive?]
  (let [[a b] (range-prefix buf inclusive?)]
    (-> buf 
        (buf-indent-lines [a (dec b)])
        save-undo)))

(defn put-from-register[buf keycode]
  (let [txt (get-register buf keycode)]
    (if (string? txt)
      (-> buf
          (buf-insert txt)
          char-backward
          save-undo)
      buf)))

(defn put-from-register-append[buf keycode]
  (let [txt (get-register buf keycode)]
    (if (string? txt)
      (let [pos (buf :pos)]
        (-> buf
            (buf-insert (inc pos) txt)
            (buf-set-pos (+ pos (count txt)))
            save-undo))
      buf)))

;(defn begin-change[buf]
;  (assoc-in buf [:context :lastpos] (buf :pos)))
;
;(defn end-change[buf putdeleted?]
;  (let [buf (save-undo buf)
;        chs (-> buf :undoes first :changes)]
;    (if (and putdeleted? (-> chs count (= 1)))
;      (let [deleted (-> chs first :to)]
;        (if-not (empty? deleted)
;          (registers-put (-> buf :context :register)
;                         deleted))))
;    (update-in buf [:context] dissoc :register)))
