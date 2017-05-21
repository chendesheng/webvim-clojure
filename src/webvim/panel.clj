;buildin panels: [Output] [Grep] [Directory] etc.
(ns webvim.panel
  (:require
    [webvim.core.event :refer [listen]]
    [me.raynes.fs :as fs]
    [clojure.pprint :refer [pprint]]
    [clojure.string :as string]
    [webvim.core.buffer :refer [buf-match-bracket
                                output-panel-name grep-panel-name ag-panel-name find-panel-name
                                directory-panel-name change-active-buffer new-file
                                get-buffers get-buf-by-path]]
    [webvim.core.rope :refer [buf-insert buf-set-pos save-undo buf-replace subr]]
    [webvim.core.line :refer [line-start move-to-line column]]
    [webvim.core.pos :refer [buf-end buf-start]]
    [webvim.core.utils :refer [shorten-path visual-size path= pretty-trace expand-path]]
    [webvim.scrolling :refer [cursor-center-viewport]]
    [webvim.core.editor :refer [update-buffer]]
    [webvim.jumplist :refer [jump-push]]))

(defn- get-panel [create? name]
  (or (if create?
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

(defn ag-panel
  ([create?]
    (get-panel create? ag-panel-name))
  ([]
    (ag-panel true)))

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

(defn goto-buf [{id :id :as buf} nextid]
  (if (or (nil? nextid) (= nextid id))
    buf
    (-> buf
        jump-push
        (change-active-buffer nextid)
        (assoc-in [:window :active-buffer] nextid))))

(defn- new-and-goto-file [buf file]
  (let [{new-bufid :id :as new-buf} (new-file file)]
    (-> buf
        (update-in [:window :buffers]
                   (fn [buffers]
                     (assoc buffers new-bufid new-buf)))
        (goto-buf new-bufid))))

(comment defn- edit-dir [path]
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
    (let [self-buf (fn [buf]
                     (if (or (empty? file) (path= file (:filepath buf)))
                       buf))
          exists-buf (fn [buf]
                       (let [next-buf (get-buf-by-path buf file)]
                         (if (some? next-buf)
                           (goto-buf buf (next-buf :id)))))
          new-buf (fn [buf]
                    (if (or new-file? (fs/exists? file))
                      (if (fs/directory? file)
                        buf  ; TODO: add back directory
                        (new-and-goto-file buf (str file)))))]
      (or (self-buf buf)
          (exists-buf buf)
          (new-buf buf)
          buf)))
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
  buf
  #_(send apanel
          (fn [buf goto?]
            (let [pos (-> buf :str count dec dec)
                  fn-set-pos (if goto? buf-set-pos (fn [buf pos] buf))]
              (-> buf
                  (buf-append s)
                  buf-end
                  line-start
                  save-undo
                  (fn-set-pos pos)
                  cursor-center-viewport
                  send-buf!
                  buf-match-bracket))) goto?)
  #_(if goto? (goto-buf buf (:id @apanel)) buf))

(defn append-output-panel [buf s goto?]
  (append-panel buf (output-panel) s goto?))

(defn- format-exception [e]
  (str e
       \newline
       (string/join "\n\t"
                    (cons "stack trace:"
                          (pretty-trace e)))
       \newline))

(listen :create-window
        (fn [window]
          ; panels is map from panel name to buffer id
          ; panel name must be unique
          (assoc window :panels {})))

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
              (assoc :beep? true))))

(listen :log
        (fn [prefix obj]
          (append-output-panel nil
                               (format "%s %s" prefix (with-out-str (pprint obj)))
                               false)
          obj))

