(ns webvim.core.buffer
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [clojure.java.io :as io])
  (:use clojure.pprint
        (clojure [string :only (join split blank?)])
        webvim.core.rope
        webvim.core.pos
        webvim.core.event
        webvim.indent
        webvim.global))

(defn buffer-list-save
  "Generate buffer id (increase from 1) and add to buffer-list"
  [b]
  (let [id (swap! gen-buf-id inc)
        b1 (assoc b :id id)]
    (reset! buffer-list (assoc @buffer-list id b1))
    b1))

(defn create-buf[bufname filepath txt]
  (let [languages {".clj" "Clojure"
                   ".js" "JavaScript"
                   ".css" "CSS"
                   ".html" "XML"
                   :else "Plain Text"}
        ext (if (nil? bufname) "" (re-find #"\.\w+$" bufname))
        ;make sure last line ends with line break
        s (if (.endsWith txt "\n") 
            (rope txt)
            (.append (rope txt) \newline))
        b {:name bufname
           ;= nil if it is a special buffer like [New File] or [Quick Fix]
           :filepath filepath 
           :str s
           :linescnt (count-lines s)
           :pos 0  ;offset from first char
           :x 0    ;saved x for up down motion
           :y 0    ;num of line breaks from first char
           ;screen scrollTop row number, update after every key press
           :scroll-top 0
           ;changes of current command, for writing back to client
           :changes [] 
           ;reverse of changes, 
           ;start record when enter insert mode (save :cursor at start)
           ;stop record when leave insert mode
           ;save to undo stack when leave insert mode
           ;contains: {:changes [c1 c2] :cursor 100}
           :pending-undo nil 
           ;undoes and redoes are stackes only push pop peek
           ;one undo contains {:changes [] :cursor}
           :undoes []
           :redoes []
           ;functions list called when :str changes. For features like autocompl need to update when :str changes
           ;get latest change from :pending-undo
           ;it's a list
           :listeners nil

           ;For client display matched braces: [{:row :col} {:row :col}]
           ;TODO set initial value
           :braces nil
           ;saved cursor when insert begins, for undo/redo function only
           :last-cursor nil
           ;unchanged
           :dirty false
           ;:type =0 visual =1 visual line =2 visual block
           ;:ranges is a vector of point pairs (unordered): [{:row :col} {:row :col}]. Always contains even number of points. Both end are inclusive
           :visual {:type 0 :ranges nil}
           ;=0 normal mode =1 insert mode =2 ex mode =3 visual mode
           :mode 0
           ;ongoing ex command
           :ex ""
           ;ongoing command keys, display beside "-- MODE --" prompt. Only save keys trigger next keymap, right before :enter function is called.
           :keys []
           ;send key to this channel when editting this buffer
           :chan-in (async/chan)
           ;get result from this channel after send key to :chan-in
           :chan-out (async/chan)

           :macro {:recording-keys nil
                   ;which register will macro save to
                   :register ""}
           ;List of highlight ranges, for hlsearch.
           :highlights nil

           ;Local registers, atom. Set init value to global registers so it can share cross buffers.
           ;Use different registers in macro replaying to avoid side effect.
           :registers registers

           :autocompl {:words nil
                       ;empty suggestions means don't display it
                       ;every input handled in insertion mode should check if :suggestion is nil.
                       ;  if it is not nil then continue narrow down suggestions
                       ;ctrl+n, ctrl+p will calculate full suggestions if it is nil
                       :suggestions nil
                       ;0 means selection nothing (don't highlight any suggestion item)
                       ;> 0 means highlight the nth suggestion item
                       :suggestions-index 0}
           ;programming language specific configs
           ;detect language by file ext name
           ;TODO detect language by file content
           :language {:name (get languages ext (languages :else))}}]
    ;(pprint (b :language))
    (fire-event b :new-buffer)))

(defn open-file
  "Create buffer from a file by slurp, return emtpy buffer if file not exists"
  [f]
  (let [nm (if (nil? f) "" (fs/base-name f))]
    (if (and (not (blank? f))
             (fs/exists? f))
      (create-buf nm f (slurp f))
      ;set :last-saved-lines make buffer start as unsaved
      (assoc (create-buf nm f "") :last-saved-lines nil))))

(defn buf-info[b]
  (if (and (empty? (b :str))
           (not (fs/exists? (b :filepath))))
    (assoc b :message (str "[New File] " (b :filepath)))
    (assoc b :message (str "\"" (:filepath b) "\""))))

;TODO make write atomic
(defn write-buffer
  [b]
  (try 
    (let [s (b :str)
          f (b :filepath)]
      (if (not (fs/exists? f))
        (do
          (-> f fs/parent fs/mkdirs)
          (-> f fs/file fs/create)))
      (spit f s)
      (-> b
          (assoc :dirty false)
          (assoc :message (str "\"" f "\" written"))))
    (catch Exception e 
      ;(println (.getMessage e))
      (.printStackTrace e)
      (let [err (str "caught exception: " (.getMessage e))]
        (assoc b :message err)))))

(defonce ^{:private true} listen-change-buffer
  (listen
    :change-buffer
    (fn [newt oldt c]
      (assoc newt :dirty true))))

(defn buffer-append-keys[b keycode]
  (assoc b :keys (conj (:keys b) keycode)))

(defn buffer-reset-keys[b]
  (assoc b :keys []))

(defn buf-copy-range[t a b inclusive]
  (let [[a b] (sort2 a b)]
    (str (subr (t :str) a (if inclusive (inc b) b)))))

(defn buf-update-highlight-brace-pair[b pos]
  (let [mpos (pos-match-brace (b :str) pos)]
    ;(println pos mpos)
    (if (nil? mpos)
      (dissoc b :braces)
      (assoc b :braces [pos mpos]))))

(defn buf-join-line
  "join current line and next line"
  [t]
  (let [pos (t :pos)
        s (t :str)
        [a b] (pos-re-forward pos s #"\n.+?(?=(\n|\S))")]
    (if (nil? a) t
      (buf-replace t a b " "))))

(defn buf-bound-scroll-top
  "Change scroll top make cursor inside viewport"
  [b]
  (let [st (-> b :scroll-top)]
    (assoc b :scroll-top 
           (let [y (b :y)
                 h (-> @window :viewport :h)]
             (cond 
               (< y st) y
               (< y (+ st h)) st
               (neg? (-> y (- h) inc)) 0
               :else (-> y (- h) inc))))))

(defn save-lastbuf[b keycode]
  (-> b (assoc-in [:context :lastbuf] b)))

(defn buf-replace-char [b ch]
  (let [pos (b :pos)]
    (buf-replace b pos (inc pos) ch)))

(defn- text-save-change[t pos from to]
  (update-in t [:changes] conj {:pos pos :len (count from) :to (str to)}))

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

(defn text-delete-range
  "delete range and set pos to end of deleted"
  [buf rg]
  (-> buf
      (buf-delete (first rg) (second rg))
      (buf-set-pos (first rg))))

(defn text-delete-offset[buf offset] 
  (let [pos (buf :pos)
        newpos (+ pos offset)]
    (if (neg? newpos) buf
      (buf-delete buf newpos))))

;;highlighting
(defn add-highlight[t rg]
  (let [highlights (t :highlights)]
    (if (empty? (filter (fn[[a b]]
                          (and (= a (rg 0)) (= b (rg 1)))) highlights))
      (update-in t [:highlights] conj rg) t)))

(defn highlight-all-matches[b re]
  (let [s (b :str)]
    (assoc b :highlights 
           (map (fn[[a b]]
                  [a (dec b)])
                (pos-re-forward-seq 0 s re)))))

(defn re-forward-highlight[t re]
  (let [pos (t :pos)
        s (t :str)
        rg (or 
             (pos-re-next-forward pos s re)
             (pos-re-forward 0 s re))] ;TODO: use region reduce duplicate work
    (if (nil? rg) t
      (let [[a b] rg]
        (-> t
            (buf-set-pos a)
            (add-highlight [a (dec b)]))))))

(defn re-backward-highlight[t re]
  (let [pos (t :pos)
        s (t :str)
        rg (or 
             (pos-re-next-backward pos s re)
             (pos-re-next-backward (count s) s re))]
    (if (nil? rg) t
      (let [[a b] rg]
        (-> t
            (buf-set-pos a)
            (add-highlight [a (dec b)]))))))

