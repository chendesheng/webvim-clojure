(ns webvim.action
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [clojure.java.io :as io])
  (:use webvim.core.line
        webvim.core.rope
        webvim.core.pos
        webvim.utils))

;one server only serve one window at one time
(defonce window (atom{:viewport {:w 0 :h 0}}))

(defonce normal-mode 0)
(defonce insert-mode 1)
(defonce visual-mode 2)
(defonce ex-mode 3)

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
  [b factor]
  (let [d (round-to-zero (* (:h (:viewport @window)) factor))
        scroll-top (b :scroll-top)
        h (-> @window :viewport :h)
        row (-> b :y)
        vrow (- row scroll-top)
        newrow (bound-range (+ row d) 0 (b :linescnt))
        newst (-> newrow (- vrow) negzero)]
    (-> b
        (assoc :scroll-top newst)
        (lines-row newrow))))

(defn cursor-center-viewport[b]
  (assoc b :scroll-top 
            (-> b :y
                (- (int (/ (-> @window :viewport :h) 2))))))

(defn buf-info[b]
  (if (and (empty? (b :str))
           (not (fs/exists? (b :filepath))))
    (assoc b :message (str "[New File] " (b :filepath)))
    (assoc b :message (str "\"" (:filepath b) "\""))))

;TODO make write atomic
(defn write-buffer
  [b]
  (try 
    (let [r (b :str)
          f (b :filepath)]
      (if (not (fs/exists? f))
        (do
          (-> f fs/parent fs/mkdirs)
          (-> f fs/file fs/create)))
      (spit f r)
      (-> b
          (assoc :dirty false)
          (assoc :message (str "\"" f "\" written"))))
    (catch Exception e 
      ;(println (.getMessage e))
      (.printStackTrace e)
      (let [err (str "caught exception: " (.getMessage e))]
        (assoc b :message err)))))

(defn quote-pattern[ch]
  (java.util.regex.Pattern/quote (str ch)))

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


(defn buf-copy-range[buf a b inclusive]
  (let [[a b] (sort2 a b)]
    (str (subr (buf :str) a (if inclusive (inc b) b)))))

(defn buf-update-highlight-brace-pair[b pos]
  (let [mpos (pos-match-brace (b :str) pos)]
    ;(println pos mpos)
    (if (nil? mpos)
      (dissoc b :braces)
      (assoc b :braces [pos mpos]))))

(defn buf-join-line
  "join current line and next line"
  [buf]
  (let [pos (buf :pos)
        r (buf :str)
        [a b] (pos-re+ r pos #"\n.+?(?=(\n|\S))")]
    (if (nil? a) buf
      (buf-replace buf a b " "))))

(defn save-lastbuf[b keycode]
  (-> b (assoc-in [:context :lastbuf] b)))

(defn buf-replace-char [b ch]
  (let [pos (b :pos)]
    (buf-replace b pos (inc pos) ch)))

(defn buf-insert
  ([buf pos txt]
   (buf-replace buf pos pos txt))
  ([buf txt]
   (buf-insert buf (buf :pos) txt)))

(defn buf-delete
  ([buf a b]
   (buf-replace buf a b ""))
  ([buf b]
   (let [pos (buf :pos)
         [a b] (sort2 pos b)]
     (buf-delete buf a b))))

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
                (pos-re-forward-seq r 0 re)))))

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

