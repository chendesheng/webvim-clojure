(ns webvim.action.edit
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [clojure.java.io :as io])
  (:use webvim.core.line
        webvim.core.rope
        webvim.core.pos
        webvim.core.serve
        webvim.core.register
        webvim.indent
        webvim.utils))

(defn buf-copy-range[buf a b inclusive]
  (let [[a b] (sort2 a b)]
    (str (subr (buf :str) a (if inclusive (inc b) b)))))

(defn buf-join-line
  "join current line and next line"
  [buf]
  (let [pos (buf :pos)
        r (buf :str)
        [a b] (pos-re+ r pos #"\n.+?(?=(\n|\S))")]
    (if (nil? a) buf
      (buf-replace buf a b " "))))

(defn save-lastbuf[buf keycode]
  (-> buf (assoc-in [:context :lastbuf] buf)))

(defn buf-replace-char [buf ch]
  (let [pos (buf :pos)]
    (buf-replace buf pos (inc pos) ch)))

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

(defn buf-delete-range
  "delete range and set pos to end of deleted"
  [buf rg]
  (-> buf
      (buf-delete (first rg) (second rg))
      (buf-set-pos (first rg))))

(defn put-from-register[buf keycode]
  (let [txt (registers-get (:registers buf) keycode)]
    (if (string? txt)
      (-> buf
          (buf-insert txt)
          char-backward
          save-undo)
      buf)))

(defn put-from-register-append[buf keycode]
  (let [txt (registers-get (:registers buf) keycode)]
    (if (string? txt)
      (let [pos (buf :pos)]
        (-> buf
            (buf-insert (inc pos) txt)
            (buf-set-pos (+ pos (count txt)))
            save-undo))
      buf)))

(defn delete-line[buf]
  (let [pos (buf :pos)
        [a b] (current-line buf)]
    (registers-put (:registers buf) (-> buf :context :register) (buf-copy-range buf a b false))
    (-> buf
        (update-in [:context] dissoc :lastbuf) ;remove :lastbuf prevent delete-motion take over.
        (buf-delete-range (current-line buf))
        line-start
        save-undo)))

(defn delete-inclusive
  [buf a b]
  (let [[a b] (sort2 a b)]
    (-> buf
        (buf-delete a (inc b))
        (buf-set-pos a))))

(defn delete-to-line-end[buf]
  (let [pos (buf :pos)
        [a b] (current-line buf)]
    (registers-put (:registers buf) (-> buf :context :register) (buf-copy-range buf pos b false))
    (-> buf
      (update-in [:context] dissoc :lastbuf) ;remove :lastbuf prevent delete-motion take over.
      (buf-delete a b)
      save-undo)))

(defn delete-range[buf]
  (let [[a b] (-> buf :visual :ranges first)]
    (-> buf
        (delete-inclusive a b)
        save-undo)))

(defn buf-delete-offset[buf offset] 
  (let [pos (buf :pos)
        newpos (+ pos offset)]
    (if (neg? newpos) buf
      (buf-delete buf newpos))))

(defn cut-char[b]
    (let [pos (b :pos)]
      (registers-put (:registers b) (-> b :context :register) (buf-copy-range b pos pos true))
      (-> b 
          (buf-delete-offset 1)
          save-undo)))

(defn yank-visual[buf]
  (let [[a b] (-> buf :visual :ranges first)]
    (registers-put (:registers buf) (-> buf :context :register) (buf-copy-range buf a b true))
    (let [[newpos  _] (sort2 a b)]
      (buf-set-pos buf newpos))))

(defn insert-line-after[buf]
  (let [pos (buf :pos)
        [_ b] (current-line buf)]
    (-> buf
        (buf-insert b <br>)
        (buf-set-pos b))))

(defn insert-line-before[buf]
  (let [pos (buf :pos)
        [a b] (current-line buf)]
    (if (zero? a)
      (-> buf
          (buf-insert 0 <br>)
          buf-start)
      (-> buf
          (buf-set-pos (- a 1))
          (buf-insert <br>)))))

(defn buf-pos-info[b]
  (let [{nm :name 
         path :filepath
         y :y} b]
    (assoc b :message (str "\"" (or path nm) "\" line " (inc y)))))

(def map-key-inclusive 
  {"h" false
   "l" false
   "w" false 
   "W" false 
   "e" true 
   "E" true 
   "b" false 
   "B" false 
   "f" true
   "F" false
   "t" true
   "T" false
   "/" false
   "$" false})

(defn inclusive? [keycode]
  (let [res (map-key-inclusive keycode)]
    (if (nil? res)
      true
      res)))

(defn buf-copy-range-lastbuf[b cur inclusive]
  (registers-put (:registers b) (-> b :context :register) (buf-copy-range b cur (b :pos) inclusive))
  b)

(defn delete-motion[b keycode]
  ;(println (str "delete-motion:" keycode))
  ;(println (str "inclusive:" (inclusive? keycode)))
  (let [lastbuf (-> b :context :lastbuf)
        pos (b :pos)]
    (if (or (nil? lastbuf) (= pos (:pos lastbuf))) ;don't do anything if cursor doesn't change
      b
      (let [inclusive (inclusive? keycode)]
        (-> lastbuf
            (buf-copy-range-lastbuf pos inclusive)
            (buf-delete (if inclusive (inc pos) pos))
            save-undo)))))

(defn yank-motion[b keycode]
  (let [lastbuf (-> b :context :lastbuf)
        lastpos (:pos lastbuf)
        pos (b :pos)]
    (if (or (nil? lastbuf) (= pos lastpos))
      b
      (let [inclusive (inclusive? keycode)]
        (buf-copy-range-lastbuf b lastpos inclusive)
        lastbuf))))

(defn indent-motion[b keycode]
  (let [lastbuf (-> b :context :lastbuf)
        lastpos (:pos lastbuf)
        pos (b :pos)]
    (if (or (nil? lastbuf) (= pos lastpos))
      b
      (-> b 
          (buf-indent-lines (sort2 pos lastpos))
          save-undo))))

