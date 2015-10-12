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

(defonce visual-normal 0)
(defonce visual-line 1)
(defonce visual-block 2) ;TODO

(defn get-register[buf c]
  (registers-get (buf :registers) c))

(defn put-register![buf c v]
  (registers-put! (buf :registers) c v))

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
  (merge buf {:mode insert-mode 
              :message nil 
              :visual {:type 0 :ranges nil}
              :keys nil}))

(defn set-normal-mode[buf]
  ;(println "set-normal-mode:")
  (merge buf {:mode normal-mode 
            :keys nil
            :visual {:type 0 :ranges nil}
            :autocompl {:suggestions nil 
                        :suggestions-index 0}}))

(defn buf-yank[buf a b linewise?]
  (let [s (buf-subr buf a b)]
    (put-register! buf (-> buf :context :register) {:str s :linewise? linewise?})
    (update-in buf [:context] dissoc :register)))

(defn change-active-buffer[id]
  (registers-put! registers "#" {:str ((active-buffer) :filepath) :id @active-buffer-id})
  (reset! active-buffer-id id)
  (registers-put! registers "%" {:str (-> @buffer-list (get id) :filepath) :id id}))


(defn make-linewise-range [[a b] buf]
  (println "make-linewise-range:" a b)
  (let [{r :str pos :pos} buf
        [a b] (sort2 a b)]
    [(pos-line-first r a) (pos-line-last r b)]))

;(make-linewise-range [14 73] {:str (rope "(ns webvim.core
;  (:require [ring.adapter.jetty :as jetty]
;            [me.raynes.fs :as fs]") :pos 14})

;collect range argument, TODO: add linewise
(defn range-prefix[buf inclusive?]
  (cond 
    (-> buf :mode (= visual-mode))
    (let [tp (-> buf :visual :type)]
      (cond 
        (= tp visual-normal)
        (-> buf :visual :ranges (get 0) (make-range inclusive?))
        (= tp visual-line)
        (-> buf :visual :ranges (get 0) (make-linewise-range buf))
        (= tp visual-block)
        (throw (Exception. "TODO: visual-block"))))
    (-> buf :context :range nil? not)
    (-> buf :context :range (make-range inclusive?))
    :else (throw (Exception. "no range prefix exist"))))

(defn change-range[buf inclusive? linewise?]
  (let [[a b] (range-prefix buf inclusive?)]
    (-> buf
        (buf-yank a b linewise?)
        (buf-delete a b)
        (set-insert-mode "c")
        (serve-keymap (-> buf :root-keymap (get "i")) "c"))))

(defn update-x[buf]
  (let [pos (buf :pos)]
    (assoc buf :x (- pos (pos-line-first (buf :str) pos)))))

(defn update-x-if-not-jk
  "update :x unless it is up down motion"
  [buf keycode]
  (let [lastbuf (buf :context :lastbuf)]
    (if-not (or (= (:pos lastbuf) (:pos buf)) 
                (contains? #{"j" "k"} keycode))
      (update-x buf) buf)))

;one server only serve one window at one time
(defonce window (atom{:viewport {:w 0 :h 0}}))

(defn cursor-center-viewport[buf]
  (assoc buf :scroll-top 
            (-> buf :y
                (- (int (/ (-> @window :viewport :h) 2))))))

(defn delete-range[buf inclusive? linewise?]
  (let [[a b] (range-prefix buf inclusive?)]
    (println "delete-range:" a b)
    (-> buf
        (buf-yank a b linewise?)
        (buf-delete a b)
        (buf-set-pos a))))

(defn yank-range[buf inclusive? linewise?]
  (let [[a b] (range-prefix buf inclusive?)]
    (buf-yank buf a b linewise?)))

(defn indent-range[buf inclusive?]
  (let [[a b] (range-prefix buf inclusive?)]
    (-> buf 
        (buf-indent-lines [a b]))))

(defn put-from-register[buf keycode]
  (let [{s :str linewise? :linewise?} (get-register buf keycode)]
    (if linewise?
      (let [{r :str pos :pos} buf
            a (pos-line-first r pos)]
        (-> buf
            (buf-insert a s)
            (buf-set-pos a)
            line-start))
      (-> buf
          (buf-insert s)
          char-backward))))

(defn put-from-register-append[buf keycode]
  (let [{s :str linewise? :linewise?} (get-register buf keycode)
        pos (buf :pos)]
    (if linewise?
      (let [r (buf :str)
            b (pos-line-last r pos)]
        (-> buf
            (buf-insert b s)
            (buf-set-pos b)
            line-start))
      (-> buf
          (buf-insert (inc pos) s)
          (buf-set-pos (+ pos (count s)))))))
