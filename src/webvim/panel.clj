;buildin panels: [Output] [Grep] [Directory] etc.
(ns webvim.panel
  (:require
    [webvim.core.event :refer [listen]]
    [me.raynes.fs :as fs]
    [clojure.pprint :refer [pprint]]
    [clojure.string :as string]
    [webvim.core.buffer :refer [output-panel-name grep-panel-name ag-panel-name
                                find-panel-name directory-panel-name
                                change-active-buffer new-file
                                get-buffers get-buf-by-path]]
    [webvim.core.rope :refer [buf-insert buf-set-pos save-undo buf-replace subr]]
    [webvim.core.line :refer [line-start move-to-line column]]
    [webvim.core.pos :refer [buf-end buf-start]]
    [webvim.core.utils :refer [shorten-path visual-size path= pretty-trace expand-path]]
    [webvim.scrolling :refer [cursor-center-viewport]]
    [webvim.core.editor :refer [async-update-other-buffer]]
    [webvim.core.lineindex :refer [range-by-line]]
    [webvim.jumplist :refer [jump-push]]))

(defn goto-buf
  ([{id :id :as buf} nextid]
    (if (or (nil? nextid) (= nextid id))
      buf
      (-> buf
          (change-active-buffer nextid)
          (assoc-in [:window :active-buffer] nextid)))))

(defn- new-and-goto-file [buf file]
  (let [{new-bufid :id :as new-buf} (new-file file)]
    (-> buf
        (assoc-in [:window :buffers new-bufid] new-buf)
        jump-push
        (goto-buf new-bufid))))

(defn- get-or-create-panel [buffers panel]
  (let [panel-buf (some #(if (-> % :name (= panel)) %) (vals buffers))]
    (if (nil? panel-buf)
      (new-file panel)
      panel-buf)))

(defn- update-panel [buf panel fn-update goto?]
  (let [panel-buf (-> buf :window :buffers (get-or-create-panel panel))
        panel-bufid (panel-buf :id)
        pos (-> panel-buf :str count dec dec)
        fn-set-pos (if goto? buf-set-pos (fn [buf _] buf))
        fn-goto-buf (if goto?
                      (fn [buf nextid]
                        (-> buf
                            jump-push
                            (goto-buf nextid)))
                      (fn [buf _] buf))]
    (if (= panel-bufid (buf :id))
      (-> buf
          fn-update
          (fn-set-pos pos))
      (-> buf
          (assoc-in [:window :buffers panel-bufid] panel-buf)
          (fn-goto-buf panel-bufid)
          (async-update-other-buffer
            panel-bufid
            (fn [buf]
              (-> buf
                  fn-update
                  (fn-set-pos pos))))))))

(defn- edit-dir [buf path]
  (let [files (string/join
                "\n"
                (map (fn [f]
                       (shorten-path (str f)))
                     (cons (fs/parent path) (fs/list-dir path))))]
    (update-panel
      buf
      directory-panel-name
      #(-> %
           (buf-replace 0 (-> % :str count) (str files "\n"))
           buf-start
           save-undo)
      true)))

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
                           (-> buf
                               jump-push
                               (goto-buf (next-buf :id))))))
          new-buf (fn [buf]
                    (if (or new-file? (fs/exists? file))
                      (if (fs/directory? file)
                        (edit-dir buf (str file))
                        (new-and-goto-file buf (-> file expand-path str)))))]
      (or (self-buf buf)
          (exists-buf buf)
          (new-buf buf)
          buf)))
  ([buf file linenum new-file?]
    (let [buf (edit-file buf file new-file?)
          nextid (-> buf :window :active-buffer)]
      (async-update-other-buffer
        buf
        nextid
        (fn [buf]
          (buf-set-pos buf
                       (-> buf :lineindex (range-by-line (dec linenum)) first)))))))

(defn- buf-append [buf & strs]
  (buf-insert
    buf
    (-> buf :str count)
    (apply str strs)))

(defn append-panel [buf panel s goto?]
  (update-panel
    buf
    panel
    #(-> %
         (buf-append s)
         buf-end
         line-start
         save-undo)
    goto?))

(defn append-output-panel [buf s goto?]
  (append-panel buf output-panel-name s goto?))

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

