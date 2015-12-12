(ns webvim.core.buffer
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [clojure.java.io :as io])
  (:use clojure.pprint
        (clojure [string :only (join split blank? lower-case)])
        webvim.core.rope
        webvim.core.pos
        webvim.core.utils
        webvim.core.parallel-universe
        webvim.core.event))

;generate buffer id and buffer id only
(defonce gen-buf-id (atom 0))

;key: buffer id, value: buffer agent
(defonce buffer-list (atom {}))

(defn buffer-list-save!
  "Generate buffer id (increase from 1) and add to buffer-list"
  [buf]
  (let [id (swap! gen-buf-id inc)
        abuf (agent (assoc buf :id id))]
    (reset! buffer-list (assoc @buffer-list id abuf))
    abuf))

(defn create-buf[bufname filepath txt]
  (let [;make sure last line ends with line break
        r (if (.endsWith txt "\n") 
            (rope txt)
            (.append (rope txt) \newline))
        buf {:name bufname
           ;= nil if it is a special buffer like [New File] or [Quick Fix]
           :filepath filepath 
           :ext (lower-case 
                  (or (re-find #"\.\w+$" (or bufname "")) ""))
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
           ;one undo contains {:changes [] :cursor}
           :history (parallel-universe)
           ;For client display matched braces: [{:row :col} {:row :col}]
           ;TODO set initial value
           :braces nil
           ;saved cursor when insert begins, for undo/redo function only
           :last-cursor nil
           ;unchanged
           :dirty false
           ;:type =0 visual =1 visual line =2 visual block
           ;:ranges is a vector of ranges (unordered): [[0 100] [101 200]]. For each range, both end are inclusive.
           :visual {:type 0 :ranges nil}
           ;=0 normal mode =1 insert mode =2 visual mode =3 ex mode
           :mode 0
           ;Ongoing command keys. This holds recent keycodes, MUST NOT changed by keymap handler directly.
           :keys nil
           ;List of highlight ranges, for hlsearch.
           :highlights nil
           ;programming language specific configs
           ;detect language by file ext name
           ;TODO detect language by file content
           :language {:id ::plain-text
                      :name "Plain Text"}
           ;:root-keymap is keymap entry of current buffer, different buffer can have different keymaps
           ;For example a directory viewer or a REPL will have it's own keymaps which is much different from the ordinary text buffer.
           ;:root-keymap is NOT an atom
           :root-keymap nil}]
    ;(pprint (buf :language))
    (-> buf
        ;make sure :new-buffer happens after languages loaded
        ;TODO: kind of ugly here
        (fire-event :load-language)
        (fire-event :new-buffer))))

(defn open-file
  "Create buffer from a file by slurp, return emtpy buffer if file not exists"
  [f]
  (if (nil? f)
    (create-buf "" "" "")
    (let [f (-> f .trim fs/normalized)]
      (create-buf (fs/base-name f) (str f) (if (fs/exists? f) 
                         (slurp f) "")))))

(defn new-file[f]
  (-> f
      open-file
      buffer-list-save!))

(defonce ^:private listen-change-buffer
  (listen
    :change-buffer
    (fn [newt oldt c]
      (assoc newt :dirty true))))

(defn printable-filepath[buf]
  (let [{nm :name
         path :filepath} buf]
    (if (nil? path) nm
      (shorten-path path))))

;TODO make write atomic
(defn write-buffer
  [buf]
  (try 
    (let [r (buf :str)
          f (buf :filepath)]
      (if (not (fs/exists? f))
        (do
          (-> f fs/parent fs/mkdirs)
          (-> f fs/file fs/create)))
      (spit f r)
      (-> buf
          (assoc :dirty false)
          (assoc :message (format "\"%s\" %dL, %dC written" (shorten-path f) (buf :linescnt) (count r)))))
    (catch Exception e 
      ;(println (.getMessage e))
      (.printStackTrace e)
      (let [err (str "caught exception: " (.getMessage e))]
        (assoc buf :message err)))))

