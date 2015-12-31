(ns webvim.keymap.motion
  (:use webvim.core.pos
        webvim.core.buffer
        webvim.core.register
        webvim.core.lang
        webvim.core.rope
        webvim.core.line
        webvim.core.event
        webvim.core.ui
        webvim.keymap.line-editor
        webvim.keymap.action
        webvim.keymap.ex
        webvim.jumplist
        webvim.core.utils))

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

(defn- re+ [buf re]
  (buf-move buf 
            (fn[r pos]
              (or (first (pos-re+ r (inc pos) re)) 
                  (-> r count dec)))))

(defn- re-[buf re]
  (buf-move buf 
            (fn[r pos]
              (or (first (pos-re- r (dec pos) re)) 0))))

(defn- word+[buf]
  (re+ buf (re-word-start-border (buf :language))))

(defn- word-[buf]
  (re- buf (re-word-start-border (buf :language))))

(defn- WORD+[buf]
  (re+ buf (re-WORD-start-border (buf :language))))

(defn- WORD-[buf]
  (re- buf (re-WORD-start-border (buf :language))))

(defn- word-end+[buf]
  (re+ buf (re-word-end-border (buf :language))))

(defn- WORD-end-[buf]
  (re+ buf (re-WORD-end-border (buf :language))))

(defn- paragraph+[buf]
  (re+ buf #"(?<=\n)\n[^\n]"))

(defn- paragraph-[buf]
  (re- buf #"((?<=\n)\n[^\n])"))

(defn- current-word[buf]
  "return range of word under cursor, right side is exclusive"
  (let [{pos :pos
         r :str
         lang :language} buf
        {word-chars :word-chars
         not-word-chars :not-word-chars} (word-re lang)
        re-start (re-pattern (str "([" not-word-chars "](?=[" word-chars "]))|((?<=[" not-word-chars "])$)"))
        re-end (re-pattern (str "[" word-chars "](?=[" not-word-chars "])"))
        b (or (last (pos-re+ r pos re-end)) (count r))
        a (or (last (pos-re- r (dec b) re-start)) 0)]
    (subr r a b)))

(defn- move-by-char[buf ch forward? inclusive?]
  (let [{r :str pos :pos} buf
        a (pos-line-first r pos)
        b (pos-line-end r pos) ;not include \n
        line (subr r a b)  ;only jump inside current line
        re (if inclusive?
             (-> ch quote-pattern re-pattern)
             (->> ch 
                  quote-pattern
                  (format (if forward? ".(?=%s)" "(?=%s)."))
                  re-pattern))
        _ (println re)
        f (if forward? pos-re+ pos-re-)
        fpos (if inclusive?  first 
               (if forward? first last))
        x (- pos a)
        newpos (-> line 
                   (f (if forward? (inc x) 
                        (if inclusive? (dec x) (- x 2))) re)
                   fpos 
                   (or x)
                   (+ a))]
    (buf-set-pos buf newpos)))

(defn- move-to-char+[buf keycode]
  (put-register! buf ";" {:str keycode :forward? true :inclusive? true})
  (move-by-char buf keycode true true))

(defn- move-to-char-[buf keycode]
  (put-register! buf ";" {:str keycode :forward? false :inclusive? true})
  (move-by-char buf keycode false true))

(defn- move-before-char+[buf keycode]
  (put-register! buf ";" {:str keycode :forward? true :inclusive? false})
  (move-by-char buf keycode true false))

(defn- move-before-char-[buf keycode]
  (put-register! buf ";" {:str keycode :forward? false :inclusive? false})
  (move-by-char buf keycode false false))

(defn- repeat-move-by-char[buf same-dir?]
  (let [{ch :str 
         forward? :forward? 
         inclusive? :inclusive?} (get-register buf ";")]
    (move-by-char buf ch (= same-dir? forward?) inclusive?)))

(defn- repeat-move-by-char+[buf]
  (repeat-move-by-char buf true))

(defn- repeat-move-by-char-[buf]
  (repeat-move-by-char buf false))

(defn- move-to-matched-braces[buf]
  (buf-move buf
            (fn [r pos]
              (pos-match-brace 
                r
                (first (pos-re+ r pos #"\(|\)|\[|\]|\{|\}"))))))

(defn- re-current-word
  "create regexp from word under cursor"
  [buf]
  (let [word (current-word buf)
        not-word-chars (-> buf :language word-re :not-word-chars)
        re-start (str "(?<=^|[" not-word-chars "])")
        re-end (str "(?=[" not-word-chars "]|$)")]
    (re-pattern
      (str re-start (quote-pattern word) re-end))))

(defn- highlight-all-matches[buf re]
  (let [r (buf :str)]
    (assoc buf :highlights 
           (map (fn[[a b]]
                  [a (dec b)])
                (pos-re-seq+ r 0 re)))))

(defn- same-word+[buf]
  (let [re (re-current-word buf)]
    (put-register! buf "/" {:str (str re) :forward? true})
    (-> buf 
        (re-forward-highlight re)
        (highlight-all-matches re))))

(defn- same-word-[buf]
  (let [re (re-current-word buf)]
    (put-register! buf "/" {:str (str re) :forward? false})
    (-> buf 
        (re-backward-highlight re)
        (highlight-all-matches re))))

(defn- same-word-first[buf]
  (let [re (re-current-word buf)]
    (put-register! buf "/" {:str (str re) :forward? true})
    (-> buf
        buf-start
        (re-forward-highlight re)
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
  (let [h (-> @ui-agent :viewport :h)
        d (round-to-zero (* h factor))
        scroll-top (buf :scroll-top)
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
  (try
    (if (empty? s) 
      ;http://stackoverflow.com/questions/1723182/a-regex-that-will-never-be-matched-by-anything
      (re-pattern "(?m)(?!x)x")
      (re-pattern (str "(?m)" s)))
    (catch Exception e 
      (re-pattern "(?m)(?!x)x"))))

(defn- increment-search-<cr>[buf]
  (let [s (-> buf :line-buffer :str str)
        prefix (-> buf :line-buffer :prefix)]
    (put-register! buf "/" {:str s :forward? (= prefix "/")})
    (-> buf
        (highlight-all-matches (search-pattern s))
        (dissoc :line-buffer))))

(defn- increment-search[buf forward?]
  (let [linebuf (buf :line-buffer)]
    (if (nil? linebuf) buf
      (let [s (-> linebuf :str str)
            re (search-pattern s)
            newbuf (-> buf :context :lastbuf (assoc :keys (buf :keys)))
            f (if forward?
                re-forward-highlight 
                re-backward-highlight)]
        ;(println (-> buf :context :lastbuf))
        ;(println "newbuf pos:"  (newbuf :pos))
        (-> newbuf
            (assoc-in [:context :lastbuf] newbuf) ;keep lastbuf for next round
            (assoc :line-buffer linebuf)
            (dissoc :highlights)
            (f re))))))

(defn- increment-search+ [buf _]
  (increment-search buf true))

(defn- increment-search- [buf _]
  (increment-search buf false))

(defn- repeat-search[buf same-dir?]
  (let[{s :str forward? :forward?} (or (get-register buf "/") 
                                      {:str "" :forward? true})
       re (search-pattern s)
       hightlightall? (-> buf :highlights empty?)
       fnsearch (if (= same-dir? forward?) re-forward-highlight re-backward-highlight)
       b1 (fnsearch buf re)] ;TODO: 1. no need fnsearch if highlight all matches. 2. cache highlight-all-matches
    (if hightlightall?
      (highlight-all-matches b1 re) b1)))

(defn- repeat-search+[buf]
  (repeat-search buf true))

(defn- repeat-search-[buf]
  (repeat-search buf false))

(defonce ^:private listen-change-buffer
  (listen 
    :change-buffer
    (fn [buf oldbuf c]
      (let [s (-> buf (get-register "/") :str str)]
        (if-not (or (-> buf :highlights empty?) (empty? s))
          (highlight-all-matches buf (search-pattern s))
          buf)))))

(defn- enter-increment-search[buf keycode]
  (-> buf
      (assoc-in [:context :lastbuf] buf)
      (assoc :line-buffer {:prefix keycode :str (rope "") :pos 0})))

(defn init-motion-keymap[line-editor-keymap]
  {"h" char-
   "l" char+
   "k" #(lines-n- % 1)
   "j" #(lines-n+ % 1)
   "g" {"g" (comp buf-start jump-push)
        "d" (comp same-word-first jump-push)}
   "G" buf-end
   "w" word+
   "W" WORD+
   "b" word-
   "B" WORD-
   "e" word-end+
   "E" WORD-end-
   "0" line-first
   "^" line-start
   "$" line-end
   "f" {:else move-to-char+}
   "F" {:else move-to-char-}
   "t" {:else move-before-char+}
   "T" {:else move-before-char-}
   ";" repeat-move-by-char+
   "," repeat-move-by-char-
   "/" (merge line-editor-keymap
              {"<cr>" increment-search-<cr>
               :after increment-search+})
   "?" (merge line-editor-keymap
              {"<cr>" increment-search-<cr>
               :after increment-search-})
   "*" same-word+
   "#" same-word-
   "n" repeat-search+
   "N" repeat-search-
   "}" paragraph+
   "{" paragraph-
   "%" move-to-matched-braces
   "<c+u>" #(cursor-move-viewport %1 -0.5) 
   "<c+d>" #(cursor-move-viewport %1 0.5)})

