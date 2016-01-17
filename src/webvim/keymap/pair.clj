(ns webvim.keymap.pair
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
        webvim.core.event
        webvim.core.lang
        webvim.indent
        webvim.core.utils
        webvim.jumplist
        webvim.autocompl))

(defn- unbalanced-bracket+[r pos re rch]
  (let [[a _] (pos-re+ r pos re)]
    (if (or (nil? a)
            (= (char-at r a) rch))
      a
      (recur r (inc (pos-match-bracket r a)) re rch))))

(defn- unbalanced-bracket-[r pos re lch]
  (let [[a _] (pos-re- r pos re)]
    (if (or (nil? a)
            (= (char-at r a) lch))
      a
      (recur r (dec (pos-match-bracket r a)) re lch))))

;(defn test-right-bracket[]
;  (unbalanced-bracket- (rope "(aaa(()()))bbb") 10 (re-pattern (quote-patterns \( \))) \())

(defn- pair-range[lch rch around?]
  (fn[buf]
    (println lch rch around?)
    (let [{r :str pos :pos} buf
          re (re-pattern (quote-patterns lch rch))
          ch (char-at r pos)
          [a b :as rg] (cond 
                         (= ch lch)
                         [pos (pos-match-bracket r pos)]
                         (= ch rch)
                         [(pos-match-bracket r pos) pos]
                         :else
                         ;FIXME: This is ugly.
                         (let [b (unbalanced-bracket+ r pos re rch)]
                           (if (nil? b) nil
                             (let [a (unbalanced-bracket- r pos re lch)]
                               (if (nil? a) nil
                                 [a b])))))]
      (if (nil? rg) buf
        (let [[_ b :as rg] (if around? rg [(inc a) (dec b)])]
          (assoc-in buf [:context :range] rg))))))

(defn- pair-quotes-range[ch around?]
  (fn[buf]
    (let [{r :str pos :pos} buf
          re (re-pattern (quote-pattern ch))
          b (first (pos-re+ r pos re))]
      (println b)
      (if (nil? b) buf
        (let [a (first (pos-re- r (dec pos) re))]
          (if (nil? a) buf
            (let [rg (if around? [a b] [(inc a) (dec b)])]
              (assoc-in buf [:context :range] rg))))))))

(defn- xml-tag-range[buf around?]
  (let [{r :str pos :pos} buf
        re #"</[^>]+>"
        [ba bb :as rgb] (pos-re+ r pos re)]
    (if (nil? rgb) buf
      (let [s (subr r [(+ ba 2) (dec bb)])
            rga (pos-re- r (dec pos) (->> s
                                          (format "<%s>")
                                          quote-pattern 
                                          re-pattern))]
        (if (nil? rga) buf
          (let [rg (if around? 
                     [(first rga) (-> rgb last dec)]
                     [(-> rga last) (-> rgb first dec)])]
            (assoc-in buf [:context :range] rg)))))))

(defn- expand-around[buf a b]
  (let [{pos :pos
         r :str
         lang :language} buf
         not-space-chars ((word-re lang) :not-space-chars)
        re (re-pattern (str "[" not-space-chars "]|\\R")) ;Java 8 new '\R' matches all line breaks
        b1 (dec (or (first (pos-re+ r (inc b) re)) (count r)))]
    (if (= b b1)
      [(inc (or (first (pos-re- r (dec a) re)) -1)) b1]
      [a b1])))

(defn current-word-range[buf around?]
  "return range of word under cursor, both sides are inclusive"
  (let [{pos :pos
         r :str
         lang :language} buf
        {word-chars :word-chars
         not-word-chars :not-word-chars} (word-re lang)
        re-start (re-pattern (str "([" not-word-chars "](?=[" word-chars "]))|((?<=[" not-word-chars "])$)"))
        re-end (re-pattern (str "[" word-chars "](?=[" not-word-chars "])"))
        b (or (first (pos-re+ r pos re-end)) (count r))
        a (or (last (pos-re- r b re-start)) 0)]
    (if around?
      (expand-around buf a b)
      [a b])))

(defn- pair-current-word[around?]
  (fn[buf]
    (let [[_ b :as rg] (current-word-range buf around?)]
      (-> buf
          (buf-set-pos b)
          (assoc-in [:context :range] rg)))))

(defn- current-WORD-range[buf around?]
  "return range of WORD under cursor, both side is inclusive"
  (let [{pos :pos
         r :str
         lang :language} buf
        {space-chars :space-chars
         not-space-chars :not-space-chars} (word-re lang)
        re-start (re-pattern (str "([" space-chars "](?=[" not-space-chars "]))|((?<=[" space-chars "])$)"))
        re-end (re-pattern (str "[" not-space-chars "](?=[" space-chars "])"))
        b (or (first (pos-re+ r pos re-end)) (count r))
        a (or (last (pos-re- r b re-start)) 0)]
    (if around?
      (expand-around buf a b)
      [a b])))

(defn- pair-current-WORD[around?]
  (fn[buf]
    (let [[_ b :as rg] (current-WORD-range buf around?)]
      (-> buf
          (buf-set-pos b)
          (assoc-in [:context :range] rg)))))

(defn- pair-keymap[around?]
  (merge (reduce-kv
           (fn[keymap lch rch]
             (-> keymap
                 (assoc (str lch) (pair-range lch rch around?))
                 (assoc (str rch) (pair-range lch rch around?))))
           {}
           brackets)
         (reduce
           (fn[keymap ch]
             (-> keymap
                 (assoc ch (pair-quotes-range ch around?))))
           {}
           ["\"" "'" "`"])
         {"t" #(xml-tag-range % around?)
          "w" (pair-current-word around?)
          "W" (pair-current-WORD around?)}))

(defn init-pair-keymap[]
  {"a" (pair-keymap true)
   "i" (pair-keymap false)})
