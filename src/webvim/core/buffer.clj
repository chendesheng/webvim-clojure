(ns webvim.core.buffer
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            [webvim.core.ui :refer [send-buf!]]
            [webvim.core.register :refer [registers-put!]])
  (:use clojure.pprint
        (clojure [string :only (join split blank? lower-case)])
        webvim.core.rope
        webvim.core.pos
        webvim.core.lang
        webvim.core.utils
        webvim.core.line
        webvim.core.parallel-universe
        webvim.core.event))

;generate buffer id and buffer id only
(defonce gen-buf-id (atom 0))

;key: buffer id, value: buffer agent
(defonce buffer-list (atom {}))

(defonce output-panel-name "[Output]")
(defonce grep-panel-name "[Grep]")
(defonce find-panel-name "[Find]")
(defonce directory-panel-name "[Directory]")

;A panel is just a speical buffer
(defonce panels #{output-panel-name grep-panel-name find-panel-name directory-panel-name})

(defn buffer-list-save!
  "Generate buffer id (increase from 1) and add to buffer-list"
  [buf]
  (let [id (swap! gen-buf-id inc)
        abuf (agent (assoc buf :id id)
                    :error-handler (fn [ui err]
                                     (println "buffer agent failed:")
                                     (println ":id " (buf :id))
                                     (println ":filepath " (buf :filepath))
                                     (println err)))]
    (reset! buffer-list (assoc @buffer-list id abuf))
    abuf))

(defn mod-time [buf]
  (if (-> buf :filepath nil?)
    0
    (-> buf :filepath fs/mod-time)))

(defn set-mod-time [buf]
  (assoc buf :mod-time (mod-time buf)))

(defn create-buf [bufname filepath txt]
  (let [txtLF (.replace txt "\r\n" "\n") ;always use LF in memory
        ;make sure last line ends with line break
        r (if (.endsWith txtLF "\n")
            (rope txtLF)
            (.append (rope txtLF) \newline))
        buf {:name bufname
             ;= nil if it is a special buffer like [New File] or [Quick Fix]
             :filepath filepath 
             :ext (lower-case 
                    (or (re-find #"\.\w+$" (or bufname "")) ""))
             :str r
             :linescnt (count-<br> r)
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
             ;For client display matched brackets: [{:row :col} {:row :col}]
             ;TODO set initial value
             :brackets nil
             ;saved cursor when insert begins, for undo/redo function only
             :last-cursor nil
             ;first one is recent undo item, second is filepath, if one of these changes then the buffer is dirty
             :save-point [nil filepath]
             ;:type =0 no-visual, =1 visual-range, =2 visual-line, =3 visual-block
             ;:ranges is a vector of ranges (unordered): [[0 100] [101 200]]. For each range, both ends are inclusive.
             ;:a and :b two ends
             :visual {:type 0 :range [0 0] :ranges nil}
             ;=0 normal mode =1 insert mode =2 ex mode
             :mode 0
             ;=0 nothing =1 temp normal mode =2 replace mode
             ;Only for displaying, like -- (insert) -- or -- Replace -- etc.
             :submode 0
             ;Ongoing command keys. This holds recent keycodes, MUST NOT changed by keymap handler directly.
             :keys nil
             ;List of highlight ranges, for hlsearch.
             :highlights nil
             ;programming language specific configs
             ;detect language by file ext name
             ;TODO detect language by file content
             :language {:id ::plain-text
                        :name "Plain Text"}
             :CRLF? (crlf? txt)
             :tabsize 4
             :expandtab false}]
    (-> buf
        set-mod-time
        init-file-type
        (fire-event :new-buffer))))

;http://stackoverflow.com/questions/13789092/length-of-the-first-line-in-an-utf-8-file-with-bom
;TODO: use Apache Commons IO: http://commons.apache.org/proper/commons-io/download_io.cgi
(defn debomify
  [^String line]
  (let [bom "\uFEFF"]
    (if (.startsWith line bom)
      (.substring line 1)
      line)))

(defn open-file
  "Create buffer from a file by slurp, return emtpy buffer if file not exists"
  [^String f]
  (if (or (nil? f) (contains? panels f)) ;TODO: handle disk files with same name. ???use negative id for these buffers???
    (create-buf (str f) nil "")
    (let [f (-> f .trim fs/normalized)]
      (create-buf (fs/base-name f)
                  (str f)
                  (if (fs/exists? f)
                    (debomify (slurp f)) "")))))

(defn new-file 
  ([^String f]
    (-> f
        open-file
        buffer-list-save!))
  ([^String f y]
    (-> f
        open-file
        (lines-row y)
        buffer-list-save!)))

(defn printable-filepath [buf]
  (let [{nm :name
         path :filepath} buf]
    (if (nil? path) nm
        (shorten-path path))))

(defn buf-pos-info [buf]
  (let [{y :y
         x :x
         linescnt :linescnt} buf
        percent (-> y inc (* 100) (/ linescnt) int)]
    (assoc buf :message (format "\"%s\" line %d of %d --%d%%-- col %d" 
                                (printable-filepath buf)
                                (inc y) linescnt percent (inc x)))))


(defn- dirty? [buf]
  (not (and 
         (-> buf :pending-undo empty?)
         (identical? (-> buf :history just-now) (-> buf :save-point first))
         (= (buf :filepath) (-> buf :save-point second)))))

(defn set-save-point [buf]
  (let [buf (assoc buf :save-point [(-> buf :history just-now) (buf :filepath)])]
    (assoc buf :dirty (dirty? buf))))

(listen :change-buffer
        (fn[buf _ _]
          (set-save-point buf)))

(defn- write-to-disk [buf]
  (let [tmp (-> buf :str str)
        s (if (buf :CRLF?) (.replace tmp "\n" "\r\n") tmp)
        f (buf :filepath)]
    (if (not (fs/exists? f))
      (do
        (-> f fs/parent fs/mkdirs)
        (-> f fs/file fs/create)))
    (spit f s)
    (-> buf
        set-save-point
        (assoc :message (format "\"%s\" %dL, %dC written" (shorten-path f) (buf :linescnt) (count s))))))

;TODO make write atomic
;TODO check disk file change
(defn write-buffer
  [buf]
  (try 
    (if (dirty? buf)
      (if (not= (buf :mod-time) (mod-time buf))
        (assoc buf :message "Error: The file has been changed since reading it.")
        (-> buf
            (fire-event :write-buffer)
            write-to-disk
            set-mod-time))
      (assoc buf :message "No changes to write"))
    (catch Exception e 
      ;(println (.getMessage e))
      (.printStackTrace e)
      (let [err (str "caught exception: " (.getMessage e))]
        (assoc buf :message err)))))

(defn file-register [buf]
  {:id (buf :id) :str (or (buf :filepath) (buf :name))})

(defn change-active-buffer [id nextid]
  (let [path-name #(or (% :filepath) (% :name))]
    (if (not= id nextid)
      (do
        (registers-put! "#" 
                        (file-register
                          (-> @buffer-list (get id) deref)))
        (registers-put! "%"
                        (file-register
                          (-> @buffer-list (get nextid) deref)))))))

(defmacro async [buf & body]
  `(let [abuf# (@buffer-list (~buf :id))]
     (-> abuf#
         (send (fn [~'buf]
                 (let [buf# ~@body]
                   (send-buf! buf#)))))
     ~buf))

(defmacro with-catch [buf & body]
  `(try
     (do ~@body)
     (catch Exception e#
       (assoc ~buf :message (str e#)))))

(defmacro async-with-catch [buf & body]
  `(async ~buf
          (with-catch ~buf ~@body)))

(defn buf-match-bracket
  ([buf pos]
    (-> buf
        (assoc :brackets [])
        (async
          (let [mpos (pos-match-bracket (buf :str) pos)]
            (assoc buf :brackets 
                   (if (nil? mpos) [] [pos mpos]))))))
  ([buf]
    (buf-match-bracket buf (buf :pos))))
