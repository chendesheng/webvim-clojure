(ns webvim.core.buffer
  (:require [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [webvim.core.lineindex :refer [create-lineindex total-lines pos-xy]]
            [webvim.core.register :refer [registers-put file-register]]
            [webvim.core.editor :refer [async-update-buffer]])
  (:use clojure.pprint
        webvim.core.rope
        webvim.core.pos
        webvim.core.lang
        webvim.core.utils
        webvim.core.line
        webvim.core.parallel-universe
        webvim.core.event))

(defn get-buffers
  ([buf]
    (-> buf :window :buffers vals))
  ([buf f]
    (filter f (get-buffers buf))))

(defmacro with-catch [buf & body]
  `(try
     (do ~@body)
     (catch Exception e#
       (fire-event e# :exception)
       (assoc ~buf :message (str e#)))))

(defonce output-panel-name "[output]")
(defonce grep-panel-name "[grep]")
(defonce ag-panel-name "[ag]")
(defonce find-panel-name "[find]")
(defonce directory-panel-name "[directory]")

;A panel is just a speical buffer
(defonce panels #{output-panel-name
                  grep-panel-name
                  find-panel-name
                  directory-panel-name
                  ag-panel-name})

;generate buffer id and buffer id only
(defonce gen-buf-id (atom 0))

(defn- wrap-agent [buf]
  (agent buf
         :error-handler (fn [ui err]
                          (println "buffer agent failed:")
                          (println ":id " (buf :id))
                          (println ":filepath " (buf :filepath))
                          (println err))))

(defn- buf-match-bracket [buf]
  (let [mpos (pos-match-bracket (buf :str) (buf :pos))]
    (assoc buf :cursor2
           (if (nil? mpos)
             []
             (-> buf :lineindex (pos-xy mpos))))))

(defn mod-time [buf]
  (if (-> buf :filepath nil?)
    0
    (-> buf :filepath fs/mod-time)))

(defn set-mod-time [buf]
  (assoc buf :mod-time (mod-time buf)))

(defn create-buf [bufname filepath txt]
  (let [txtLF (-> txt
                  (.replace "\r\n" "\n")
                  ensure-ends-with-newline) ;always use LF in memory
        lineindex (create-lineindex txtLF)
        buf {:id (swap! gen-buf-id inc)
             :name bufname
             ;= nil if it is a special buffer like [New File] or [Quick Fix]
             :filepath filepath
             :ext (string/lower-case
                    (or (re-find #"\.\w+$" (or bufname "")) ""))
             :str (rope txtLF)
             :lineindex lineindex
             :lines (total-lines lineindex)
             :pos 0  ;offset from first char
             :x 0    ;saved x for up down motion
             :y 0    ;num of line breaks from first char
             :cursor [0 0]
             ;screen scrollTop row number, update after every key press
             :scroll-top 0
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
             ;first one is recent undo item, second is filepath, if either of these changes then the buffer is dirty
             :save-point [nil filepath]
             :dirty false
             ;:ranges is a vector of ranges (unordered): [[0 100] [101 200]]. For each range, both ends are inclusive.
             ;:a and :b two ends
             :visual {:type :no-visual :range [0 0] :ranges nil :ranges2 nil}
             ;=0 normal mode =1 insert mode =2 ex mode
             :mode :normal-mode
             ;=0 nothing =1 temp normal mode =2 replace mode
             ;Only for displaying, like -- (insert) -- or -- Replace -- etc.
             :submode :none
             ;Ongoing command keys. This holds recent keycodes, MUST NOT changed by keymap handler directly.
             :keys nil
             ;List of highlight ranges, for hlsearch.
             :highlights nil
             :highlights2 nil ;for cljs client
             ;programming language specific configs
             ;detect language by file ext name
             ;TODO detect language by file content
             :language {:id ::plain-text
                        :name "Plain Text"}
             :CRLF? (crlf? txt)
             :tabsize 4
             :expandtab false
             :view nil}]
    (-> buf
        set-mod-time
        init-file-type
        buf-match-bracket ;FIXME: this could be slow
        (fire-event :new-buffer))))

;http://stackoverflow.com/questions/13789092/length-of-the-first-line-in-an-utf-8-file-with-bom
;TODO: use Apache Commons IO: http://commons.apache.org/proper/commons-io/download_io.cgi
(defn debomify
  [^String line]
  (let [bom "\uFEFF"]
    (if (.startsWith line bom)
      (.substring line 1)
      line)))

(defn new-file
  ([^String f]
    (if (or (empty? f) (contains? panels f)) ;TODO: handle disk files with same name. ???use negative id for these buffers???
      (create-buf f nil "")
      (let [f (-> f .trim fs/normalized)]
        (create-buf (fs/base-name f)
                    (str f)
                    (if (fs/exists? f)
                      (debomify (slurp f)) "")))))
  ([^String f y]
    (-> f
        new-file
        (lines-row y))))

(defn printable-filepath [buf]
  (let [{nm :name
         path :filepath} buf]
    (if (nil? path) nm
        (shorten-path path))))

(defn- column-str [buf]
  (let [r (buf :str)
        pos (buf :pos)
        col (inc (column buf))]
    (if (= (char-at r pos) \tab)
      (format "%d-%d" (- col (-> buf :tabsize dec)) col)
      (str col))))

(defn buf-pos-info [buf]
  (let [y (buf :y)
        linescnt (buf-total-lines buf)
        percent (-> y inc (* 100) (/ linescnt) int)]
    (assoc buf :message (format "\"%s\" line %d of %d --%d%%-- col %s"
                                (printable-filepath buf)
                                (inc y) linescnt percent (column-str buf)))))

(defn set-save-point [buf]
  (let [buf (assoc buf :save-point [(-> buf :history just-now) (buf :filepath)])]
    (assoc buf :dirty false)))

(listen :change-buffer
        (fn [buf _ _]
          (async-update-buffer
            buf
            (fn [buf]
              (assoc buf :dirty
                     (or
                       (-> buf :pending-undo empty? not)
                       (not (identical? (-> buf :history just-now) (-> buf :save-point first)))
                       (not= (buf :filepath) (-> buf :save-point second))))))))

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
        (assoc :message (format "\"%s\" %dL, %dC written" (shorten-path f) (buf-total-lines buf) (count s))))))

;TODO make write atomic
;TODO check disk file change
(defn write-buffer
  ([buf force?]
    (try
      (cond
        force?
        (-> buf
            (fire-event :write-buffer)
            write-to-disk
            set-mod-time)
        (buf :dirty)
        (if (not= (buf :mod-time) (mod-time buf))
          (assoc buf :message "Error: The file has been changed since reading it.")
          (-> buf
              (fire-event :write-buffer)
              write-to-disk
              set-mod-time))
        :else
        (assoc buf :message "No changes to write"))
      (catch Exception e
        (fire-event e :exception)
        (-> buf
            (assoc :message (str e))
            (assoc :beep? true)))))
  ([buf]
    (write-buffer buf false)))

(def persistent-buffers #(-> % :filepath some?))
(def temp-buffers #(-> % :filepath nil?))

(defn get-buffer-by-filepath [buf filepath]
  (first (filter
           (fn [buf]
             (= (buf :filepath) filepath)) (get-buffers buf))))

(defn change-active-buffer [{bufid :id :as buf} nextid]
  (let [buf (if (-> buf :window :buffers (get nextid) :view nil?)
              (-> buf
                  (assoc-in [:window :buffers nextid :view] (buf :view))
                  (dissoc :view))
              buf)
        put (partial update-in buf [:window :registers] registers-put)]
    (if (and (some? nextid) (not= bufid nextid))
      (put "#" (file-register (-> buf :window :buffers (get bufid))))
      (put "%" (file-register (-> buf :window :buffers (get nextid)))))))

(listen :buffer-changed
        (fn [buf old-buf {winid :id}]
          (if (or (not= (:str old-buf) (:str buf))
                  (not= (:pos old-buf) (:pos buf)))
            (do
              (async-update-buffer winid (buf :id) buf-match-bracket)
              (assoc buf :cursor2 []))
            buf)))

(defn get-buf-by-path [buf path]
  (->> buf :window :buffers vals
       (some #(if (and (-> % :filepath some?)
                       (path= path (% :filepath))) %))))

