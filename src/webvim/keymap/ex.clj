(ns webvim.keymap.ex
  (:require [me.raynes.fs :as fs]
            [snipsnap.core :as clipboard]
            [clojure.core.async :as async]
            [cheshire.core :as json]
            [ring.adapter.jetty9 :as jetty]
            [clojure.string :as string])
  (:use clojure.pprint
        webvim.core.rope
        webvim.core.line
        webvim.core.buffer
        webvim.core.register
        webvim.core.pos
        webvim.jumplist
        webvim.core.utils
        webvim.core.event
        webvim.fuzzy
        webvim.keymap.external
        webvim.keymap.compile
        webvim.keymap.action))

(defn- find-buffer [buffers f]
  (reduce-kv
    (fn [matches _ abuf]
      (let [buf @abuf
            indexes (fuzzy-match (buf :name) f)]
        (if (empty? indexes)
          matches
          (conj matches buf))))
    [] buffers))

(defn- buf-append[buf & strs]
  (buf-insert 
    buf
    (-> buf :str count)
    (apply str strs)))

(defn- exec-shell-commands[buf cmds]
  (let [aoutputbuf (output-buf true)]
    (async/go
      (let [res (apply clojure.java.shell/sh cmds)
            s (if (empty? (res :out))
                (res :err) (res :out))]
        ;(println s)
        (send aoutputbuf 
              (fn[buf]
                ;(println "grepbufid:" (buf :id))
                (let [row (-> buf :linescnt)
                      newbuf (-> buf
                                 (buf-append (string/join " " cmds) "\n" s "\n")
                                 (move-to-line row)
                                 cursor-center-viewport
                                 (bound-scroll-top ""))]
                  (jetty/send! @ws-out (json/generate-string (webvim.render/render buf newbuf)))
                  (assoc newbuf :changes []))))))
    (goto-buf buf aoutputbuf)))

(defn- write-output[buf s goto?]
  (let [aoutputbuf (output-buf true)]
    (send aoutputbuf
          (fn[buf]
            (let [newbuf (-> buf
                             (buf-append s "\n" "\n")
                             buf-end
                             cursor-center-viewport)]
              (jetty/send! @ws-out (json/generate-string (webvim.render/render buf newbuf)))
              (assoc newbuf :changes []))))
    (if goto? (goto-buf buf aoutputbuf) buf)))

(defn cmd-write [buf _ file]
  (if (not (string/blank? file))
    (-> buf
        (assoc :name (fs/base-name file))
        (assoc :filepath file)
        write-buffer)
    (if (nil? (buf :filepath))
      (assoc buf :message "No file name")
      (write-buffer buf))))

(defn cmd-buffer [buf execmd file]
  (let [matches (find-buffer @buffer-list file)
        cnt (count matches)
        equals (filter #(= (% :name) file) matches)]
    (cond
      (= (count equals) 1)
      (let[id (buf :id)
           nextid (-> equals first :id)]
        (if (not= id nextid)
          (let[]
            (jump-push buf)
            (change-active-buffer id nextid)))
        (assoc buf :nextid nextid))
      (= 0 (count matches))
      (assoc buf :message "No file match")
      (= 1 (count matches))
      (let[id (buf :id)
           nextid (-> matches first :id)]
        (if (not= id nextid)
          (let []
            (jump-push buf)
            (change-active-buffer id nextid)))
        (assoc buf :nextid nextid))
      (> (count matches) 1)
      ;display matched buffers at most 5 buffers
      (assoc buf :message (str "which one? " (string/join ", " (map :name (take 5 matches))))))))

(defn cmd-bnext[buf execmd args]
  (let [id (buf :id)
        nextid (or
        ;get next id larger than current
                 (->> @buffer-list (map #(-> % val deref :id)) (filter #(> % id)) sort first)
                 (-> @buffer-list first val deref :id))]
                 ;(println "nextid:" nextid)
    (if (not (= nextid id))
      (do
        (change-active-buffer id nextid)
        (jump-push buf)))
    (assoc buf :nextid nextid)))

(defn cmd-bprev[buf execmd args]
  (let [id (buf :id)
        nextid (or
                 (->> @buffer-list (map #(-> % val deref :id)) (filter #(> % id)) sort first)
                 (-> @buffer-list first val deref :id))]
    (if (not (= nextid id))
      (do
        (change-active-buffer id nextid)
        (jump-push buf)))
    (assoc buf :nextid nextid)))

(defn cmd-bdelete[buf execmd args]
  (swap! buffer-list dissoc (buf :id))
  (let [nextbuf (or (@registers "#") @(new-file nil))
        firstbuf (-> @buffer-list first val deref)
        nextid (nextbuf :id)]
    (registers-put! registers "%" {:id nextid :str (nextbuf :filepath)})
    (if (or (nil? firstbuf) (= (firstbuf :id) nextid))
      (registers-put! registers "#" nil)
      (registers-put! registers "#" {:id (firstbuf :id) :str (firstbuf :filepath)}))
    (assoc buf :nextid nextid)))

(defn cmd-eval[buf execmd args]
  (try (->> args read-string eval str
            (assoc buf :message))
       (catch Exception e
              (assoc buf :message (str e)))))

(defn cmd-grep[buf execmd args]
  (exec-shell-commands buf ["grep" "-rnI" args "."]))

(defn cmd-find[buf execmd args]
  (exec-shell-commands buf ["find" "." "-name" args]))

(defn cmd-move-to-line[buf row _]
;(println "row:" row)
  (jump-push buf)
  (let [row (bound-range (dec (Integer. row)) 0 (-> buf :linescnt dec))]
    (move-to-line buf row)))

(defn cmd-ls[buf execmd args]
  (write-output buf
                (str ":ls\n"
                     (string/join 
                       "\n" 
                       (map (fn[abuf]
                              (let [buf @abuf]
                                (str (buf :id) ":" " " (or (buf :filepath) (buf :name)))))
                            (vals @buffer-list)))) true))

(defn cmd-nohl[buf _ _] 
  (assoc buf :highlights []))

(defn cmd-edit[buf excmd file]
  (edit-file buf file true))

(defn- ex-commands[]
  (let [cmds 
        [["write" cmd-write]
         ["nohlsearch" cmd-nohl]
         ["edit" cmd-edit]
         ["buffer" cmd-buffer]
         ["bnext" cmd-bnext]
         ["bprev" cmd-bprev]
         ["bdelete" cmd-bdelete]
         ["eval" cmd-eval]
         ["grep" cmd-grep]
         ["find" cmd-find]
         [#"^(\d+)$" cmd-move-to-line]
         ["ls" cmd-ls]]]
        (fire-event cmds :init-ex-commands)))

(defn- execute [buf cmds]
  (let [[_ excmd args] (re-find #"^\s*([^\s]+)\s*(.*)\s*$"
                                (-> buf :line-buffer :str str))]
    (if (nil? excmd)
      buf
      (let [handlers (filter fn?
                             (map (fn[[cmd handler]]
                                    ;(println cmd)
                                    (if (string? cmd)
                                      (if (zero? (.indexOf cmd excmd)) handler nil)
                                      (let [m (re-find cmd excmd)]
                                        (if (not (nil? m)) handler nil)))) cmds))]
        (println excmd args)
        (if (>= (count handlers) 1)
          ((first handlers) buf excmd args)
          (assoc buf :message "unknown command"))))))

(defn- ex-tab-complete [{{r :str} :line-buffer :as buf} cmds]
  (if (re-test #"^\s*\S+\s*$" r)
    (let [s (str r)
          news (first
                 (filter
                   (fn[k]
                     (and (string? k)
                          (.startWith k s)))
                   (map first cmds)))]
      (if (nil? news) buf
        (update-in buf [:line-buffer]
                   (fn[linebuf]
                     (merge linebuf {:str (rope news) :pos (count news)})))))
    buf))

(defn- append-<br>[buf]
  (let [s (-> buf :line-buffer :str)
        len (count s)
        news (replacer s len len <br>)]
    (assoc-in buf [:line-buffer :str] news)))

(defn init-ex-mode-keymap[line-editor-keymap]
  (let [cmds (ex-commands)]
    (merge line-editor-keymap
           {:enter (fn[buf keycode]
                     (-> buf
                         ((line-editor-keymap :enter) keycode)
                         (assoc :mode ex-mode)))
            "<cr>" (fn[buf]
                     (-> buf
                         ;append a <br> indicates this command is already executed
                         append-<br>
                         (execute cmds)))
            "<tab>" (fn[buf]
                      (ex-tab-complete buf cmds))})))
