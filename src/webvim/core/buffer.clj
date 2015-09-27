(ns webvim.core.buffer
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [clojure.java.io :as io])
  (:use clojure.pprint
        (clojure [string :only (join split blank?)])
        webvim.core.rope
        webvim.core.pos
        webvim.core.event))

;generate buffer id and buffer id only
(defonce gen-buf-id (atom 0))

;key: buffer id, value: buffer map
(defonce buffer-list (atom {}))

(defn buffer-list-save
  "Generate buffer id (increase from 1) and add to buffer-list"
  [b]
  (let [id (swap! gen-buf-id inc)
        b1 (assoc b :id id)]
    (reset! buffer-list (assoc @buffer-list id b1))
    b1))

;the current editting buffer, when receving keys from client send keycode to this buffer's :chan-in
;switch to another buffer is very easy, just do (reset! actvive-buffer b)
;TODO Serve more than one client at once, make eash session different active-buffer
(defonce active-buffer-id (atom int))

(defn active-buffer[]
  (@buffer-list @active-buffer-id))

(defn create-buf[bufname filepath txt]
  (let [languages {".clj" "Clojure"
                   ".js" "JavaScript"
                   ".css" "CSS"
                   ".html" "XML"
                   :else "Plain Text"}
        ext (if (nil? bufname) "" (re-find #"\.\w+$" bufname))
        ;make sure last line ends with line break
        r (if (.endsWith txt "\n") 
            (rope txt)
            (.append (rope txt) \newline))
        b {:name bufname
           ;= nil if it is a special buffer like [New File] or [Quick Fix]
           :filepath filepath 
           :str r
           :linescnt (count-lines r)
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
           ;List of highlight ranges, for hlsearch.
           :highlights nil
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

(defonce ^{:private true} listen-change-buffer
  (listen
    :change-buffer
    (fn [newt oldt c]
      (assoc newt :dirty true))))

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

(def word-chars "a-zA-Z_\\-!.?+*=<>&#\\':0-9")
(def not-word-chars (str "^" word-chars))
(def space-chars "\\s,")
(def not-space-chars "^\\s,")
(def punctuation-chars (str "^" word-chars space-chars))
(def not-punctuation-chars (str word-chars space-chars))

