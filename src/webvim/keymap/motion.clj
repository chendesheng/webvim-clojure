(ns webvim.keymap.motion
  (:use webvim.core.pos
        webvim.core.buffer
        webvim.core.register
        webvim.core.lang
        webvim.core.rope
        webvim.core.line
        webvim.core.event
        webvim.keymap.line-editor
        webvim.keymap.action
        webvim.keymap.ex
        webvim.jumplist
        webvim.utils))

(defn- re-word-start-border[lang]
  (let [res (word-re lang)]
  (re-pattern 
    (str "(?<=[" (res :not-word-chars) "])[" (res :word-chars) "]|(?<=[" (res :not-punctuation-chars) "])[" (res :punctuation-chars) "]"))))

(defn- re-WORD-start-border[lang]
  (let [res (word-re lang)]
    (re-pattern 
      (str "(?<=[" (res :space-chars) "])[" (res :not-space-chars) "]"))))

(defn- re-word-end-border[lang]
  (let [res (word-re lang)]
    (re-pattern 
      (str "[" (res :word-chars) "](?=[" (res :not-word-chars) "])|[" (res :punctuation-chars) "](?=[" (res :not-punctuation-chars) "])"))))

(defn- re-WORD-end-border[lang]
  (let [res (word-re lang)]
    (re-pattern 
      (str "[" (res :not-space-chars) "](?=[" (res :space-chars) "])"))))

(defn- re-forward [buf re]
  (buf-move buf 
            (fn[r pos]
              (or (first (pos-re+ r (inc pos) re)) 
                  (-> r count dec)))))

(defn- re-backward[buf re]
  (buf-move buf 
            (fn[r pos]
              (or (first (pos-re- r (dec pos) re)) 0))))

(defn- word-forward[buf]
  (re-forward buf (re-word-start-border (buf :language))))

(defn- word-backward[buf]
  (re-backward buf (re-word-start-border (buf :language))))

(defn- WORD-forward[buf]
  (re-forward buf (re-WORD-start-border (buf :language))))

(defn- WORD-backward[buf]
  (re-backward buf (re-WORD-start-border (buf :language))))

(defn- word-end-forward[buf]
  (re-forward buf (re-word-end-border (buf :language))))

(defn- WORD-end-forward[buf]
  (re-forward buf (re-WORD-end-border (buf :language))))

(defn- paragraph-forward[buf]
  (re-forward buf #"(?<=\n)\n[^\n]"))

(defn- paragraph-backward[buf]
  (re-backward buf #"((?<=\n)\n[^\n])"))

(defn- pos-word[lang r pos]
  (let [{word-chars :word-chars
         not-word-chars :not-word-chars} (word-re lang)
        re-start (re-pattern (str "([" not-word-chars "](?=[" word-chars "]))|((?<=[" not-word-chars "])$)"))
        re-end (re-pattern (str "[" word-chars "](?=[" not-word-chars "])"))
        b (or (last (pos-re+ r pos re-end)) (count r))
        a (or (last (pos-re- r (dec b) re-start)) 0)]
      [a b]))

;(pos-word (rope "aaa") 2)

(defn- current-word[buf]
  "return range of word under cursor, right side is exclusive"
  (let [{pos :pos
         r :str} buf]
    ;(println pos r)
    (pos-word (buf :language) r pos)))

(defn- move-to-next-char[buf keycode]
  (let [ch (keycode-to-char keycode)]
    (re-forward buf (-> ch quote-pattern re-pattern))))

(defn- move-to-back-char[buf keycode]
  (let [ch (keycode-to-char keycode)]
    (re-backward buf (-> ch quote-pattern re-pattern))))

(defn- move-before-next-char[buf keycode]
  (-> buf (move-to-next-char keycode)
      char-backward))

(defn- move-after-back-char[buf keycode]
  (-> buf (move-to-back-char keycode)
      char-backward))

(defn- move-to-matched-braces[buf]
  (buf-move buf
            (fn [r pos]
              (pos-match-brace 
                r
                (first (pos-re+ r pos #"\(|\)|\[|\]|\{|\}"))))))

(defn- left-boundary[lang] 
  (str "(?<=^|[" (-> lang word-re :not-word-chars) "])"))
(defn- right-boundary[lang] 
  (str "(?=[" (-> lang word-re :not-word-chars) "]|$)"))

(defn- highlight-all-matches[buf re]
  (let [r (buf :str)]
    (assoc buf :highlights 
           (map (fn[[a b]]
                  [a (dec b)])
                (pos-re-seq+ r 0 re)))))

(defn- move-next-same-word[buf]
  (let [[start end] (current-word buf)
        word (subr (buf :str) start end)
        ;_ (println (str word))
        re (re-pattern (str (left-boundary buf) (quote-pattern word) (right-boundary buf)))]
    (registers-put (:registers buf) "/" (str "/" re))
    (-> buf 
        (re-forward-highlight re)
        (highlight-all-matches re))))

(defn- move-back-same-word[buf]
  (let [[start end] (current-word buf)
        word (subr (buf :str) start end)
        re (re-pattern (str (left-boundary buf) (quote-pattern word) (right-boundary buf)))]
    (registers-put (:registers buf) "/" (str "?" re))
    (-> buf 
        (re-backward-highlight re)
        (highlight-all-matches re))))

(defn- round-to-zero
  "(round-to-zero -9.1) = -9; (round-to-zero 9.1) = 9"
  [i]
  (if (> i 0)
    (int i)
    (- (int (- i)))))

(defn- negzero[n]
  (if (neg? n) 0 n))

(defn- cursor-move-viewport
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

(defn- search-pattern[s]
  (re-pattern (str "(?m)" s)))

(defn- handle-search[buf]
  (let [s (-> buf :line-buffer :str str)
        prefix (-> buf :line-buffer :prefix)]
    (registers-put (:registers buf) "/" (str prefix s))
    (-> buf
        (highlight-all-matches (search-pattern s))
        (dissoc :line-buffer))))

(defn- increment-search[buf f]
  (let [linebuf (buf :line-buffer)]
    (if (nil? linebuf) buf
      (let [s (-> linebuf :str str)
            re (search-pattern s)
            newbuf (-> buf :context :lastbuf)]
        ;(println (-> buf :context :lastbuf))
        ;(println "newbuf pos:"  (newbuf :pos))
        (-> newbuf
            (assoc-in [:context :lastbuf] newbuf) ;keep lastbuf for next round
            (assoc :line-buffer linebuf)
            (dissoc :highlights)
            (f re))))))

(defn- repeat-search[buf dir]
  (let[s (or (registers-get (:registers buf) "/") "/")
       re (search-pattern (subs s 1))
       hightlightall? (-> buf :highlights empty?)
       fnsearch (if (= (first s) dir) re-forward-highlight re-backward-highlight)
       b1 (fnsearch buf re)] ;TODO: 1. no need fnsearch if highlight all matches. 2. cache highlight-all-matches
    (if hightlightall?
      (highlight-all-matches b1 re) b1)))

(defn- repeat-search+[buf]
  (repeat-search buf \/))

(defn- repeat-search-[buf]
  (repeat-search buf \?))

(defonce ^{:private true} listen-change-buffer
  (listen 
    :change-buffer
    (fn [buf oldbuf c]
      (let [re (-> buf :registers deref (get "/" "/") (subs 1) re-pattern)]
        (if-not (or (-> buf :highlights empty?) (-> re str empty?))
          (highlight-all-matches buf re)
          buf)))))

(defn- enter-increment-search[buf keycode]
  (-> buf
      (assoc-in [:context :lastbuf] buf)
      (assoc :line-buffer {:prefix keycode :str (rope "") :pos 0})))

(defn- continue-increment-search[buf keycode]
  (not (contains? #{"<cr>" "<esc>"} keycode)))

(defn init-motion-keymap[ex-mode-keymap line-editor-keymap]
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
   ;   "/" (merge ex-mode-keymap ;TODO: This is not ex-mode, add line edit mode
   ;              {"<cr>" handle-search})
   "/" (merge line-editor-keymap
              {:enter enter-increment-search
               "<cr>" handle-search
               "<esc>" #(-> % :context :lastbuf)
               :after (fn[buf keycode] (increment-search buf re-forward-highlight))
               :continue continue-increment-search})
   "?" (merge line-editor-keymap
              {:enter enter-increment-search
               "<cr>" handle-search
               "<esc>" #(-> % :context :lastbuf)
               :after (fn[buf keycode] (increment-search buf re-backward-highlight))
               :continue continue-increment-search})
   "*" move-next-same-word
   "#" move-back-same-word
   "n" repeat-search+
   "N" repeat-search-
   "}" paragraph-forward
   "{" paragraph-backward
   "%" move-to-matched-braces
   "<c+u>" #(cursor-move-viewport %1 -0.5) 
   "<c+d>" #(cursor-move-viewport %1 0.5)})

