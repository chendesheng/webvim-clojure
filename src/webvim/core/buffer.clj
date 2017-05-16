(ns webvim.core.buffer
  (:require [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [webvim.core.ui :refer [send-buf!]]
            [webvim.core.lineindex :refer [create-lineindex total-lines pos-xy]]
            [webvim.core.register :refer [registers-put!]]
            [webvim.core.editor :refer [*window*]])
  (:use clojure.pprint
        webvim.core.rope
        webvim.core.pos
        webvim.core.lang
        webvim.core.utils
        webvim.core.line
        webvim.core.parallel-universe
        webvim.core.event))

(defn- buffer-list []
  (*window* :buffers))

(defn get-buffer-agent [id]
  (@(buffer-list) id))

(defn get-buffer-by-id [id]
  (let [abuf (get-buffer-agent id)]
    (if (nil? abuf)
      nil
      @abuf)))

(defn get-buffer-agent-by-name [name]
  (some (fn [abuf]
          (if (= (@abuf :name) name)
            abuf))
        (vals @(buffer-list))))

(defn get-buffers
  ([]
    (map deref (vals @(buffer-list))))
  ([f]
    (filter f (get-buffers))))

(defn do-buffers [f]
  (doseq [abuf (vals @(buffer-list))]
    (send abuf f)))

(defn remove-buffer [id]
  (swap! (buffer-list) dissoc id))

(defn reset-buffers! []
  (reset! (buffer-list) {}))

(defmacro with-catch [buf & body]
  `(try
     (do ~@body)
     (catch Exception e#
       (fire-event e# :exception)
       (assoc ~buf :message (str e#)))))

(defmacro async [buf & body]
  `(let [abuf# (get-buffer-agent (~buf :id))]
     (if (some? abuf#)
       (-> abuf#
           (send (fn [~'buf]
                   (let [buf# ~@body]
                     (send-buf! buf#))))))
     ~buf))

(defmacro async-with-catch [buf & body]
  `(async ~buf
          (with-catch ~buf ~@body)))

(defonce output-panel-name "[Output]")
(defonce grep-panel-name "[Grep]")
(defonce ag-panel-name "[ag]")
(defonce find-panel-name "[Find]")
(defonce directory-panel-name "[Directory]")

;A panel is just a speical buffer
(defonce panels #{output-panel-name grep-panel-name find-panel-name directory-panel-name})

;generate buffer id and buffer id only
(defonce gen-buf-id (atom 0))

(defn- wrap-agent [buf]
  (agent buf
         :error-handler (fn [ui err]
                          (println "buffer agent failed:")
                          (println ":id " (buf :id))
                          (println ":filepath " (buf :filepath))
                          (println err))))

(defn- buffer-list-save!
  "Generate buffer id (increase from 1) and add to buffer-list"
  [buf]
  (let [abuf (wrap-agent buf)]
    (swap! (buffer-list)
           (fn [buffers abuf]
             (assoc buffers (@abuf :id) abuf)) abuf)
    abuf))

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
             :lines 1
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
             :expandtab false}]
    (-> buf
        set-mod-time
        init-file-type
        (fire-event :new-buffer))))

(listen :create-window
        (fn [window]
          (println "buffer create-window")
          (let [buf (create-buf "" nil "")
                areg (window :registers)]
            (swap! areg (fn [reg]
                          (assoc reg "%" (buf :name))))
            (send (window :ui) (fn [ui] (assoc ui :buf buf)))
            (assoc window
                   :buffers
                   (atom {(buf :id) (wrap-agent buf)})))))

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
          (async buf
                 (assoc buf :dirty
                        (or
                          (-> buf :pending-undo empty? not)
                          (not (identical? (-> buf :history just-now) (-> buf :save-point first)))
                          (not= (buf :filepath) (-> buf :save-point second)))))))

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

(defn file-register [buf]
  {:id (buf :id) :str (or (buf :filepath) (buf :name))})

(def persistent-buffers #(-> % :filepath some?))
(def temp-buffers #(-> % :filepath nil?))

(defn get-buffer-by-filepath [filepath]
  (first (filter
           (fn [buf]
             (= (buf :filepath) filepath)) (get-buffers))))

(defn update-buffer [id f & args]
  (let [abuf (get-buffer-agent id)]
    (if (nil? abuf)
      nil
      (apply send abuf f args))))

(defn change-active-buffer [id nextid]
  (if (and (some? nextid) (not= id nextid))
    (do
      (registers-put! "#"
                      (file-register (get-buffer-by-id id)))
      (registers-put! "%"
                      (file-register (get-buffer-by-id nextid))))))

(defn buf-match-bracket
  ([buf pos]
    (-> buf
        (assoc :brackets [])
        (async
          (let [lidx (buf :lineindex)
                mpos (pos-match-bracket (buf :str) pos)]
            (if (nil? mpos)
              (-> buf
                  (assoc :brackets [])
                  (assoc :cursor2 []))
              (-> buf
                  (assoc :brackets [pos mpos])
                  (assoc :cursor2 (pos-xy lidx mpos))))))))
  ([buf]
    (buf-match-bracket buf (buf :pos))))
