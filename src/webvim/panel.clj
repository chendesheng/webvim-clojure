;buildin panels: [Output] [Grep] [Directory] etc.
(ns webvim.panel
  (:require
    [me.raynes.fs :as fs]
    [clojure.string :as str]
    [webvim.core.buffer :refer [buffer-list open-file buffer-list-save! 
                                output-panel-name grep-panel-name find-panel-name 
                                directory-panel-name change-active-buffer new-file]]
    [webvim.core.rope :refer [buf-insert buf-set-pos save-undo buf-replace subr]]
    [webvim.core.line :refer [line-start pos-line-first lines-row move-to-line column]]
    [webvim.core.pos :refer [buf-end buf-start]]
    [webvim.core.utils :refer [shorten-path visual-size]]
    [webvim.core.ui :refer [send-buf!]]
    [webvim.scrolling :refer [cursor-center-viewport]]
    [webvim.jumplist :refer [jump-push]]))

(defn- path= [f1 f2]
  (try
    (= (str (fs/normalized f1))
       (str (fs/normalized f2)))
    (catch Exception ex
      (println ex)
      false)))

(defn- get-panel [create? name]
  (or (some (fn [[_ abuf]]
              (if (= (@abuf :name) name) abuf nil))
            @buffer-list)
      (if create?
        (-> (open-file name)
            buffer-list-save!))))

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

(defn goto-buf [buf nextbuf]
  (let [nextid (:id nextbuf)
        id (buf :id)]
    (if (or (nil? nextid) (= nextid id)) buf
        (do (change-active-buffer id nextid)
            (jump-push buf)
            (assoc buf :nextid nextid)))))

(defn- edit-dir [path]
  (let [abuf (directory-panel)
        files (str/join "\n"
                        (map (fn [f] (-> f str shorten-path))
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
                             (->> @buffer-list vals (map deref)))
            file (str (fs/expand-home file))
            newbuf (if (nil? buf-exists)
                     (if (or new-file? (fs/exists? file))
                       (if (fs/directory? file)
                         (edit-dir file)
                         (-> file str new-file deref)))
                     buf-exists)]
        (if (or (nil? newbuf) (= (buf :id) (newbuf :id))) buf
            (let [newid (newbuf :id)]
              (change-active-buffer (buf :id) newid)
              (jump-push buf)
              (assoc buf :nextid newid))))))
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
        (let [anextbuf (@buffer-list nextid)]
          (send anextbuf (fn [buf row]
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
                (buf-append s "\n")
                buf-end
                line-start
                save-undo
                (fn-set-pos pos)
                cursor-center-viewport
                send-buf!))) goto?)
  (if goto? (goto-buf buf @apanel) buf))

(defn append-output-panel [buf s goto?]
  (append-panel buf (output-panel) s goto?))
