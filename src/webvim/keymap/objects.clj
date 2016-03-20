(ns webvim.keymap.objects
  (:use webvim.core.rope
        webvim.core.line
        webvim.core.pos
        webvim.core.lang
        webvim.core.utils))

(defn- set-range [buf [a b]]
  (if (= a b)
    buf
    (let [b (dec b)]
      (-> buf
          (buf-set-pos a)
          (assoc-in [:context :range] [a b])))))

(defn- word-range [around? f]
  (fn [buf keycode]
    (set-range buf (f buf around?))))

(defn- unbalanced-bracket+ [r pos re rch]
  (let [[a _] (pos-re+ r pos re)]
    (if (or (nil? a)
            (= (char-at r a) rch))
      a
      (recur r (inc (pos-match-bracket r a)) re rch))))

(defn- unbalanced-bracket- [r pos re lch]
  (let [[a _] (pos-re- r pos re)]
    (if (or (nil? a)
            (= (char-at r a) lch))
      a
      (recur r (dec (pos-match-bracket r a)) re lch))))

;(defn test-right-bracket[]
;  (unbalanced-bracket- (rope "(aaa(()()))bbb") 10 (re-pattern (quote-patterns \( \))) \())

(defn- bracket-range [lch rch around?]
  (fn [buf keycode]
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
          (let [[a b :as rg] (if around? rg [(inc a) (dec b)])]
            (-> buf
                (buf-set-pos a)
                (assoc-in [:context :range] rg)))))))

(defn- quotes-range [ch around?]
  (fn [buf keycode]
    (let [{r :str pos :pos} buf
          re (re-pattern (quote-pattern ch))
          b (last (pos-re+ r pos re))]
      ;(println b)
      (if (nil? b) buf
          (let [a (first (pos-re- r (dec pos) re))]
            (if (nil? a) buf
                (set-range buf (if around? [a b] [(inc a) (dec b)]))))))))

(defn- xml-tag-range [r pos]
  (let [re #"<(?!\!--)/?([^>\s]+)[^>\r\n]*>"
        open-tag? (fn [a] (not= (char-at r (inc a)) \/))
        close-tags (filter (fn [[a b]]
                             (if (and (<= a pos) (< pos b))
                               (not (open-tag? a))
                               (< pos b)))
                           (pos-re-seq+ r (pos-line-first r pos) re))
        same-tag? (fn [[sa sb] [ea eb]]
                    (println (str (subr r sa sb)))
                    (println (str (subr r ea eb)))
                    (= (second (re-seq re (subr r sa sb)))
                       (second (re-seq re (subr r ea eb)))))
        unbalance-tag (fn [tags open-tag? same-tag?]
                        (loop [[[a _ :as rg] & rest] tags
                               stack []] 
                          (cond
                            (nil? rg) nil
                            (open-tag? a) (recur rest (conj stack rg))
                            (empty? stack) rg
                            (same-tag? (peek stack) rg) (recur rest (pop stack))
                            :else nil)))
        close-tag (unbalance-tag close-tags open-tag? same-tag?)]
    (if-not (nil? close-tag)
      (let [open-tags (filter
                        (fn [[a b]]
                          (if (and (<= a pos) (< pos b))
                            (open-tag? a)
                            true))
                        (pos-re-seq- r pos re))
            open-tag (unbalance-tag open-tags 
                                    (complement open-tag?)
                                    (fn [a b]
                                      (same-tag? b a)))]
        (if-not (nil? open-tag)
          [open-tag close-tag])))))

(defn- xml-tag-range-handler [around?]
  (fn [buf keycode]
    (let [[[sa sb] [ea eb] :as matched] (xml-tag-range (buf :str) (buf :pos))]
      (println matched)
      (if (nil? matched) buf
          (set-range buf (if around?
                           [sa eb] [sb ea]))))))

(defn- expand-around [buf a b]
  (let [{pos :pos
         r :str
         lang :language} buf
        not-space-chars ((word-re lang) :not-space-chars)
        re (re-pattern (str "[" not-space-chars "]|\\r?\\n"))
        b1 (or (first (pos-re+ r b re)) (count r))]
    [a b1]))

(defn- current-range [buf word-chars not-word-chars around?]
  (let [{pos :pos
         r :str} buf
        re-start (re-pattern (str "([" not-word-chars "](?=[" word-chars "]))|((?<=[" not-word-chars "])$)"))
        re-end (re-pattern (str "[" word-chars "](?=[" not-word-chars "])"))
        b (or (last (pos-re+ r pos re-end)) (count r))
        a (or (last (pos-re- r (dec b) re-start)) 0)]
    (println a b)
    (if around?
      (expand-around buf a b)
      [a b])))

(defn- current-word-range [buf around?]
  (let [{word-chars :word-chars
         not-word-chars :not-word-chars} (word-re (buf :language))]
    (current-range buf word-chars not-word-chars around?)))

(defn- current-WORD-range [buf around?]
  (let [{space-chars :space-chars
         not-space-chars :not-space-chars} (word-re (buf :language))]
    (current-range buf not-space-chars space-chars around?)))

(defn current-word [buf]
  (subr (buf :str) (current-word-range buf false)))

(defn current-WORD [buf]
  (subr (buf :str) (current-WORD-range buf false)))

(defn- objects-keymap [around?]
  (merge (reduce-kv
           (fn [keymap lch rch]
             (-> keymap
                 (assoc (str lch) (bracket-range lch rch around?))
                 (assoc (str rch) (bracket-range lch rch around?))))
           {}
           brackets)
         (reduce
           (fn [keymap ch]
             (-> keymap
                 (assoc ch (quotes-range ch around?))))
           {}
           ["\"" "'" "`"])
         {"t" (xml-tag-range-handler around?)
          "w" (word-range around? current-word-range)
          "W" (word-range around? current-WORD-range)}))

(defn init-objects-keymap []
  {"a" (objects-keymap true)
   "i" (objects-keymap false)})
