(ns webvim.action.motion
  (:use webvim.core.pos
        webvim.core.buffer
        webvim.jumplist
        webvim.utils
        webvim.core.register
        webvim.core.lang
        webvim.core.rope))

(defn re-word-start-border[lang]
  (let [res (word-re lang)]
  (re-pattern 
    (str "(?<=[" (res :not-word-chars) "])[" (res :word-chars) "]|(?<=[" (res :not-punctuation-chars) "])[" (res :punctuation-chars) "]"))))

(defn re-WORD-start-border[lang]
  (let [res (word-re lang)]
    (re-pattern 
      (str "(?<=[" (res :space-chars) "])[" (res :not-space-chars) "]"))))

(defn re-word-end-border[lang]
  (let [res (word-re lang)]
    (re-pattern 
      (str "[" (res :word-chars) "](?=[" (res :not-word-chars) "])|[" (res :punctuation-chars) "](?=[" (res :not-punctuation-chars) "])"))))

(defn re-WORD-end-border[lang]
  (let [res (word-re lang)]
    (re-pattern 
      (str "[" (res :not-space-chars) "](?=[" (res :space-chars) "])"))))

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
  (re-forward buf (re-word-start-border (buf :language))))

(defn word-backward[buf]
  (re-backward buf (re-word-start-border (buf :language))))

(defn WORD-forward[buf]
  (re-forward buf (re-WORD-start-border (buf :language))))

(defn WORD-backward[buf]
  (re-backward buf (re-WORD-start-border (buf :language))))

(defn word-end-forward[buf]
  (re-forward buf (re-word-end-border (buf :language))))

(defn WORD-end-forward[buf]
  (re-forward buf (re-WORD-end-border (buf :language))))

(defn paragraph-forward[buf]
  (re-forward buf #"(?<=\n)\n[^\n]"))

(defn paragraph-backward[buf]
  (re-backward buf #"((?<=\n)\n[^\n])"))

(defn pos-word[lang r pos]
  (let [{word-chars :word-chars
         not-word-chars :not-word-chars} (word-re lang)
        re-start (re-pattern (str "([" not-word-chars "](?=[" word-chars "]))|((?<=[" not-word-chars "])$)"))
        re-end (re-pattern (str "[" word-chars "](?=[" not-word-chars "])"))
        b (or (last (pos-re+ r pos re-end)) (count r))
        a (or (last (pos-re- r (dec b) re-start)) 0)]
      [a b]))

;(pos-word (rope "aaa") 2)

(defn current-word[buf]
  "return range of word under cursor, right side is exclusive"
  (let [{pos :pos
         r :str} buf]
    ;(println pos r)
    (pos-word (buf :language) r pos)))

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

(defn move-to-matched-braces[buf]
  (buf-move buf
            (fn [r pos]
              (pos-match-brace 
                r
                (first (pos-re+ r pos #"\(|\)|\[|\]|\{|\}"))))))

(defn add-highlight[buf rg]
  (let [highlights (buf :highlights)]
    (if (empty? (filter (fn[[a b]]
                          (and (= a (rg 0)) (= b (rg 1)))) highlights))
      (update-in buf [:highlights] conj rg) buf)))

(defn highlight-all-matches[b re]
  (let [r (b :str)]
    (assoc b :highlights 
           (map (fn[[a b]]
                  [a (dec b)])
                (pos-re-seq+ r 0 re)))))

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

(defn- left-boundary[lang] 
  (str "(?<=^|[" (-> lang word-re :not-word-chars) "])"))
(defn- right-boundary[lang] 
  (str "(?=[" (-> lang word-re :not-word-chars) "]|$)"))

(defn move-next-same-word[buf]
  (let [[start end] (current-word buf)
        word (subr (buf :str) start end)
        ;_ (println (str word))
        re (re-pattern (str (left-boundary buf) (quote-pattern word) (right-boundary buf)))]
    (registers-put (:registers buf) "/" (str "/" re))
    (-> buf 
        (re-forward-highlight re)
        (highlight-all-matches re))))

(defn move-back-same-word[buf]
  (let [[start end] (current-word buf)
        word (subr (buf :str) start end)
        re (re-pattern (str (left-boundary buf) (quote-pattern word) (right-boundary buf)))]
    (registers-put (:registers buf) "/" (str "?" re))
    (-> buf 
        (re-backward-highlight re)
        (highlight-all-matches re))))

;TODO: only highlight diff parts
;(defonce listen-change-buffer
;  (fn [newt oldt c]
;    (let [a1 (c :pos)
;          b1 (+ a1 (c :len))
;          highlights (oldt :highlights) 
;          {insects :insects 
;           not-insects :not-insects} (partition-by 
;                                       (fn[[a b]] 
;                                         (if (or (<= a a1 b) 
;                                                 (<= a b1 b))
;                                           :insects 
;                                           :not-insects)) highlights)
;          newhighlights (reduce 
;                          (fn [[a b]]
;                            ) nil insects)]
;      (assoc newt
;             :highlights (cons not-insects newhighlights)))))

(defn handle-search[b]
  (registers-put (:registers b) "/" (b :ex))
  (-> b
      (highlight-all-matches (re-pattern (str "(?m)" (subs (b :ex) 1))))
      (assoc :ex "")))

(defn- repeat-search[buf reg]
  (let[s (or (registers-get (:registers buf) "/") reg)
       dir (first s)
       re (re-pattern (str "(?m)" (subs s 1)))
       hightlightall? (-> buf :highlights empty?)
       fnsearch (if (= \/ dir) re-forward-highlight re-backward-highlight)
       b1 (fnsearch buf re)] ;TODO: 1. no need fnsearch if highlight all matches. 2. cache highlight-all-matches
    (if hightlightall?
      (highlight-all-matches b1 re) b1)))

(defn repeat-search+[buf]
  (repeat-search buf "/"))

(defn repeat-search-[buf]
  (repeat-search buf "?"))

