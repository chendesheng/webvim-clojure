(ns webvim.action.motion
  (:use webvim.core.pos
        webvim.core.buffer
        webvim.jumplist
        webvim.utils
        webvim.core.register
        webvim.core.rope))

(def re-word-start-border
  (re-pattern 
    (str "(?<=[" not-word-chars "])[" word-chars "]|(?<=[" not-punctuation-chars "])[" punctuation-chars "]")))

(def re-WORD-start-border
  (re-pattern 
    (str "(?<=[" space-chars "])[" not-space-chars "]")))

(def re-word-end-border
  (re-pattern 
    (str "[" word-chars "](?=[" not-word-chars "])|[" punctuation-chars "](?=[" not-punctuation-chars "])")))

(def re-WORD-end-border
  (re-pattern 
    (str "[" not-space-chars "](?=[" space-chars "])")))

(defn re-forward [buf re]
  (buf-move buf 
            (fn[r pos]
              (or (first (pos-re+ r (inc pos) re)) 
                  (-> r count dec)))))

(defn re-backward[buf re]
  (buf-move buf 
            (fn[r pos]
              (or (first (pos-re- r (dec pos) re)) 0))))

(defn word-forward[buf]
  (re-forward buf re-word-start-border))

(defn word-backward[buf]
  (re-backward buf re-word-start-border))

(defn WORD-forward[buf]
  (re-forward buf re-WORD-start-border))

(defn WORD-backward[buf]
  (re-backward buf re-WORD-start-border))

(defn word-end-forward[buf]
  (re-forward buf re-word-end-border))

(defn WORD-end-forward[buf]
  (re-forward buf re-WORD-end-border))

(defn paragraph-forward[buf]
  (re-forward buf #"(?<=\n)\n[^\n]"))

(defn paragraph-backward[buf]
  (re-backward buf #"((?<=\n)\n[^\n])"))

(defn pos-word[r pos]
  (let [re-start (re-pattern (str "([" not-word-chars "](?=[" word-chars "]))|((?<=[" not-word-chars "])$)"))
        re-end (re-pattern (str "[" word-chars "](?=[" not-word-chars "])"))
        b (or (last (pos-re+ r pos re-end)) (count r))
        a (or (last (pos-re- (dec b) r re-start)) 0)]
      [a b]))

;(pos-word (rope "aaa") 2)

(defn current-word[buf]
  "return range of word under cursor, right side is exclusive"
  (let [{pos :pos
         r :str} buf]
    ;(println pos r)
    (pos-word r pos)))

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
                     (pos-re-forward-seq r pos re)
                     (pos-re-backward-seq r pos re))
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

(defn move-to-next-char[buf keycode]
  (let [ch (keycode-to-char keycode)]
    (re-forward buf (-> ch quote-pattern re-pattern))))

(defn move-to-back-char[buf keycode]
  (let [ch (keycode-to-char keycode)]
    (re-backward buf (-> ch quote-pattern re-pattern))))

(defn move-before-next-char[buf keycode]
  (-> buf (move-to-next-char keycode)
      char-backward))

(defn move-after-back-char[buf keycode]
  (-> buf (move-to-back-char keycode)
      char-backward))

(defn change-active-buffer[id]
  (registers-put registers "#" @active-buffer-id)
  (reset! active-buffer-id id)
  (registers-put registers "%" id))

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
