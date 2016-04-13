;buildin panels: [Output] [Grep] [Directory] etc.
(ns webvim.panel
  (:require
    [webvim.core.event :refer [listen]]
    [me.raynes.fs :as fs]
    [clojure.pprint :refer [pprint]]
    [clojure.string :as string]
    [webvim.core.buffer :refer [update-buffer
                                output-panel-name grep-panel-name find-panel-name 
                                directory-panel-name change-active-buffer new-file
                                get-buffers get-buffer-agent-by-name]]
    [webvim.core.rope :refer [buf-insert buf-set-pos save-undo buf-replace subr]]
    [webvim.core.line :refer [line-start pos-line-first lines-row move-to-line column]]
    [webvim.core.pos :refer [buf-end buf-start]]
    [webvim.core.utils :refer [shorten-path visual-size path= pretty-trace expand-path]]
    [webvim.core.ui :refer [send-buf!]]
    [webvim.scrolling :refer [cursor-center-viewport]]
    [webvim.jumplist :refer [jump-push]]))

(defn- get-panel [create? name]
  (or (get-buffer-agent-by-name name)
      (if create?
        (new-file name))))

(defn output-panel
  ([create?]
    (get-panel create? output-panel-name))
  ([]
    (output-panel true)))

(defn grep-panel
  ([create?]
    (get-panel create? grep-panel-name))
  ([]
    (grep-panel true)))

(defn find-panel
  ([create?]
    (get-panel create? find-panel-name))
  ([]
    (find-panel true)))

(defn directory-panel
  ([create?]
    (get-panel create? directory-panel-name))
  ([]
    (directory-panel true)))

(defn goto-buf [buf nextid]
  (let [id (buf :id)]
    (if (or (nil? nextid) (= nextid id)) buf
        (do (change-active-buffer id nextid)
            (jump-push buf)
            (assoc buf :nextid nextid)))))

(defn- edit-dir [path]
  (let [abuf (directory-panel)
        files (string/join "\n"
                           (map (fn [f]
                                  (shorten-path (str f)))
                                (cons (fs/parent path) (fs/list-dir path))))]
    @(send abuf
           (fn [buf]
             (-> buf
                 (buf-replace 0 (-> buf :str count) (str files "\n"))
                 buf-start
                 save-undo
                 send-buf!)))))

(defn- update-x [buf]
  (assoc buf :x (column buf)))

(defn edit-file
  ([buf file new-file?]
    (if (or (empty? file) (path= file (:filepath buf)))
      buf
      (let [buf-exists (some #(if (and (-> % :filepath nil? not)
                                       (path= file (% :filepath))) %)
                             (get-buffers))
            file (str (expand-path file))
            newbuf (if (nil? buf-exists)
                     (if (or new-file? (fs/exists? file))
                       (if (fs/directory? file)
                         (edit-dir file)
                         (-> file str new-file deref)))
                     buf-exists)]
        (if (nil? newbuf)
          buf
          (goto-buf buf (newbuf :id))))))
  ([buf file linenum new-file?]
    (let [newbuf (edit-file buf file new-file?)
          nextid (newbuf :nextid)
          row (dec linenum)]
      (if (nil? nextid)
        (if (<= row 0) buf
            (-> buf
                jump-push
                (move-to-line (dec row))
                update-x))
        (do
          (println "goto row:" row)
          (update-buffer nextid
                         (fn [buf row]
                           (if (<= row 0) buf
                               (-> buf
                                   (move-to-line row)
                                   update-x))) row)
          newbuf)))))

(defn- buf-append [buf & strs]
  (buf-insert 
    buf
    (-> buf :str count)
    (apply str strs)))

(defn append-panel [buf apanel s goto?]
  (send apanel
        (fn [buf goto?]
          (let [pos (-> buf :str count dec)
                fn-set-pos (if goto? buf-set-pos (fn [buf pos] buf))]
            (-> buf
                (buf-append s)
                buf-end
                line-start
                save-undo
                (fn-set-pos pos)
                cursor-center-viewport
                (send-buf! goto?)))) goto?)
  (if goto? (goto-buf buf (:id @apanel)) buf))

(defn append-output-panel [buf s goto?]
  (append-panel buf (output-panel) s goto?))

(defn- format-exception [e]
  (str e
       \newline
       (string/join "\n\t"
                    (cons "stack trace:"
                          (pretty-trace e)))
       \newline))

(listen :exception
        (fn [e]
          (.printStackTrace e)
          (append-output-panel nil (format-exception e) false)))

;Error indicates serious problem.
;Error means bug.
(listen :error
        (fn [buf e]
          (.printStackTrace e)
          (-> buf
              (append-output-panel (format-exception e) true)
              (assoc :beep true))))

(listen :log
        (fn [prefix obj]
          (append-output-panel nil
                               (format "%s %s" prefix (with-out-str (pprint obj)))
                               false)
          obj))

