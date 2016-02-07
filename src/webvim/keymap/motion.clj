(ns webvim.keymap.motion
  (:use webvim.core.pos
        webvim.core.buffer
        webvim.core.register
        webvim.core.lang
        webvim.core.rope
        webvim.core.line
        webvim.core.event
        webvim.core.ui
        webvim.core.parallel-universe
        webvim.keymap.linebuf.linebuf
        webvim.keymap.action
        webvim.keymap.ex
        webvim.jumplist
        webvim.keymap.search
        webvim.core.utils))

(defn- re-cw[lang]
  (let [res (word-re lang)]
    (re-pattern 
      (str "(?<=[" (res :space-chars) "])[" (res :not-space-chars) "]|"
           "(?<=[" (res :word-chars) "])[" (res :not-word-chars) "]|"
           "(?<=[" (res :punctuation-chars) "])[" (res :not-punctuation-chars) "]"))))

(defn re-cW[lang]
  (let [res (word-re lang)]
    (re-pattern 
      (str "(?<=[" (res :space-chars) "])[" (res :not-space-chars) "]|"
           "(?<=[" (res :not-space-chars) "])[" (res :space-chars) "]"))))

(defn cw-move[buf keycode]
  (re+ buf (re-cw (buf :language))))

(defn cW-move[buf keycode]
  (re+ buf (re-cW (buf :language))))

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

(defn- word+[buf keycode]
  (re+ buf (re-word-start-border (buf :language))))

(defn- word-[buf keycode]
  (re- buf (re-word-start-border (buf :language))))

(defn- WORD+[buf keycode]
  (re+ buf (re-WORD-start-border (buf :language))))

(defn- WORD-[buf keycode]
  (re- buf (re-WORD-start-border (buf :language))))

(defn- word-end+[buf keycode]
  (re+ buf (re-word-end-border (buf :language))))

(defn- WORD-end+[buf keycode]
  (re+ buf (re-WORD-end-border (buf :language))))

(defn- word-end-[buf keycode]
  (re- buf (re-word-end-border (buf :language))))

(defn- WORD-end-[buf keycode]
  (re- buf (re-WORD-end-border (buf :language))))

(defn- paragraph+[buf keycode]
  (re+ buf #"(?<=\n)\n[^\n]"))

(defn- paragraph-[buf keycode]
  (re- buf #"((?<=\n)\n[^\n])"))

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
        ;_ (println re)
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
  (registers-put! ";" {:str keycode :forward? true :inclusive? true})
  (move-by-char buf keycode true true))

(defn- move-to-char-[buf keycode]
  (registers-put! ";" {:str keycode :forward? false :inclusive? true})
  (move-by-char buf keycode false true))

(defn- move-before-char+[buf keycode]
  (registers-put! ";" {:str keycode :forward? true :inclusive? false})
  (move-by-char buf keycode true false))

(defn- move-before-char-[buf keycode]
  (registers-put! ";" {:str keycode :forward? false :inclusive? false})
  (move-by-char buf keycode false false))

(defn- repeat-move-by-char[buf same-dir?]
  (let [{ch :str 
         forward? :forward? 
         inclusive? :inclusive?} (registers-get ";")]
    (move-by-char buf ch (= same-dir? forward?) inclusive?)))

(defn- repeat-move-by-char+[buf keycode]
  (repeat-move-by-char buf true))

(defn- repeat-move-by-char-[buf keycode]
  (repeat-move-by-char buf false))

(defn- move-to-matched-brackets[buf keycode]
  (buf-move buf
            (fn [r pos]
              (let [[a _] (pos-re+ r pos re-brackets)]
                (if (nil? a) pos
                  (pos-match-bracket r a))))))

(defn- re-current-word
  "create regexp from word under cursor"
  [buf]
  (let [word (current-word buf)
        not-word-chars (-> buf :language word-re :not-word-chars)
        re-start (str "(?<=^|[" not-word-chars "])")
        re-end (str "(?=[" not-word-chars "]|$)")]
    (re-pattern
      (str re-start (quote-pattern word) re-end))))

(defn- same-word[forward?]
  (fn[buf keycode]
    (let [re (re-current-word buf)
          s (str re)]
      (registers-put! "/" {:str s :forward? forward?})
      (-> buf 
          (search-message s forward?)
          (re-forward-highlight re)
          (highlight-all-matches re)))))

(defn- same-word-first[buf]
  (let [re (re-current-word buf)
        s (str re)]
    (registers-put! "/" {:str s :forward? true})
    (-> buf
        buf-start
        (search-message s true)
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
  [factor]
  (fn[buf keycode]
    (let [h (-> @ui-agent :viewport :h)
          d (round-to-zero (* h factor))
          scroll-top (buf :scroll-top)
          row (-> buf :y)
          vrow (- row scroll-top)
          newrow (bound-range (+ row d) 0 (buf :linescnt))
          newst (-> newrow (- vrow) negzero)]
      (-> buf
          (assoc :scroll-top newst)
          (lines-row newrow)))))

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

(defn- repeat-search[buf same-dir?]
  (let[{s :str forward? :forward?} (or (registers-get "/") 
                                       {:str "" :forward? true})
       re (search-pattern s)
       hightlightall? (-> buf :highlights empty?)
       fnsearch (if (= same-dir? forward?) re-forward-highlight re-backward-highlight)
       buf (-> buf
               (fnsearch re)
               (search-message s (= same-dir? forward?)))] ;TODO: 1. no need fnsearch if highlight all matches. 2. cache highlight-all-matches
    (if hightlightall?
      (highlight-all-matches buf re) buf)))

(defn- repeat-search+[buf keycode]
  (repeat-search buf true))

(defn- repeat-search-[buf keycode]
  (repeat-search buf false))

(defonce ^:private listen-change-buffer
  (listen 
    :change-buffer
    (fn [buf oldbuf c]
      (let [s (:str (registers-get "/"))]
        (if-not (or (-> buf :highlights empty?) (empty? s))
          (highlight-all-matches buf (search-pattern s))
          buf)))))

(defn- viewport-position[percentFromTop]
  (fn[buf keycode]
    (move-to-line buf 
                  (+ (buf :scroll-top)
                     (-> @ui-agent :viewport :h dec (* percentFromTop) Math/ceil)))))

(defn init-motion-keymap[]
  {"h" (wrap-keycode char-)
   "l" (wrap-keycode char+)
   "k" (wrap-keycode #(lines-n % -1))
   "j" (wrap-keycode #(lines-n % 1))
   "g" {"g" (wrap-keycode (comp buf-start jump-push))
        "d" (wrap-keycode (comp same-word-first jump-push))
        "e" word-end-
        "E" WORD-end-}
   "G" (fn[buf keycode]
         (if (-> buf :context :repeat-prefix nil? not)
           (lines-row buf (dec (repeat-prefix-value buf)))
           (-> buf buf-end line-start)))
   "H" (viewport-position 0)
   "M" (viewport-position 0.5)
   "L" (viewport-position 1)
   "w" word+
   "W" WORD+
   "b" word-
   "B" WORD-
   "e" word-end+
   "E" WORD-end+
   "0" (wrap-keycode line-first)
   "^" (wrap-keycode line-start)
   "$" (wrap-keycode line-end)
   "f" {:else move-to-char+}
   "F" {:else move-to-char-}
   "t" {:else move-before-char+}
   "T" {:else move-before-char-}
   ";" repeat-move-by-char+
   "," repeat-move-by-char-
   "/" (increment-search-keymap true)
   "?" (increment-search-keymap false)
   "*" (same-word true)
   "#" (same-word false)
   "n" repeat-search+
   "N" repeat-search-
   "}" paragraph+
   "{" paragraph-
   "%" move-to-matched-brackets
   "<c-u>" (cursor-move-viewport -0.5) 
   "<c-d>" (cursor-move-viewport 0.5)})
