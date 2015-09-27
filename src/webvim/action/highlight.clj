(ns webvim.action.highlight
  (:use webvim.core.pos
        webvim.core.rope
        webvim.core.line
        webvim.core.buffer
        webvim.core.register
        webvim.action.motion
        webvim.utils))

;;highlighting
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

(def left-boundary (str "(?<=^|[" not-word-chars "])"))
(def right-boundary (str "(?=[" not-word-chars "]|$)"))

(defn move-next-same-word[b]
  (let [[start end] (current-word b)
        word (subr (b :str) start end)
        ;_ (println (str word))
        re (re-pattern (str left-boundary (quote-pattern word) right-boundary))]
    (registers-put (:registers b) "/" (str "/" re))
    (-> b 
        (re-forward-highlight re)
        (highlight-all-matches re))))

(defn move-back-same-word[b]
  (let [[start end] (current-word b)
        word (subr (b :str) start end)
        re (re-pattern (str left-boundary (quote-pattern word) right-boundary))]
    (registers-put (:registers b) "/" (str "?" re))
    (-> b 
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

