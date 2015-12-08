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

(defonce ^:private output-buf-name "*output*")

(defn- output-buf[]
  (or (some (fn[[_ abuf]]
              (if (= (@abuf :name) output-buf-name) abuf nil))
            @buffer-list)
      (-> (open-file output-buf-name)
          (assoc :root-keymap @root-keymap)
          buffer-list-save!)))

(defn- exec-shell-commands[buf cmds]
  (let [aoutputbuf (output-buf)
        nextid (@aoutputbuf :id)]
    (async/go
      (let [
            res (apply clojure.java.shell/sh cmds)
            s (if (empty? (res :out))
                (res :err) (res :out))]
        (println s)
        (send aoutputbuf 
              (fn[buf]
                ;(println "grepbufid:" (buf :id))
                (let [row (-> buf :linescnt)
                      newbuf (-> buf
                                 buf-end
                                 (buf-insert (str (string/join " " cmds) "\n"))
                                 buf-end
                                 (buf-insert (str s "\n"))
                                 (move-to-line (dec row))
                                 cursor-center-viewport
                                 (bound-scroll-top ""))]
                  (jetty/send! @ws-out (json/generate-string (webvim.render/render buf newbuf)))
                  (assoc newbuf :changes []))))))
    (change-active-buffer (buf :id) nextid)
    (jump-push buf)
    (assoc buf :nextid nextid)))

(defn- ex-commands[motion-keymap]
  (array-map
    "write" (fn[buf _ file]
              (if (not (string/blank? file))
                (-> buf
                    (assoc :name (fs/base-name file))
                    (assoc :filepath file)
                    write-buffer)
                (if (nil? (buf :filepath))
                  (assoc buf :message "No file name")
                  (write-buffer buf))))
    "nohlsearch" (fn[buf _ _]
                   (assoc buf :highlights []))
    "edit" (fn[buf excmd file]
             (edit-file buf file true))
    "buffer" (fn [buf execmd file]
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
    "bnext" (fn[buf execmd args]
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
    "bprev" (fn[buf execmd args]
              (let [id (buf :id)
                    nextid (or
                             (->> @buffer-list (map #(-> % val deref :id)) (filter #(> % id)) sort first)
                             (-> @buffer-list first val deref :id))]
                (if (not (= nextid id))
                  (do
                    (change-active-buffer id nextid)
                    (jump-push buf)))
                (assoc buf :nextid nextid)))
    "bdelete" (fn[buf execmd args]
                (swap! buffer-list dissoc (buf :id))
                (let [nextbuf (or (@registers "#") @(new-file nil))
                      firstbuf (-> @buffer-list first val deref)
                      nextid (nextbuf :id)]
                  (registers-put! registers "%" {:id nextid :str (nextbuf :filepath)})
                  (if (or (nil? firstbuf) (= (firstbuf :id) nextid))
                    (registers-put! registers "#" nil)
                    (registers-put! registers "#" {:id (firstbuf :id) :str (firstbuf :filepath)}))
                  (assoc buf :nextid nextid)))
    "eval" (fn[buf execmd args]
             (->> args
                  read-string
                  eval
                  str
                  (assoc buf :message)))
    "reload" (fn[buf execmd args] ;just for webvim itself TODO: move to dev/user.clj
               (let [[[_ nm]] (re-seq #"src/(.+)\.clj" (buf :filepath))
                     code (str "(use '" (string/replace nm "/" ".") " :reload)")
                     ret (->> code read-string eval)]
                 (if (nil? ret)
                   (assoc buf :message (user/restart))
                   (assoc buf :message (str ret)))))
    "grep" (fn[buf execmd args]
             (exec-shell-commands buf ["grep" "-rnI" args "."]))
    "find" (fn[buf execmd args]
             (exec-shell-commands buf ["find" "." "-name" args]))
    #"^(\d+)$" (fn[buf row _]
                 ;(println "row:" row)
                 (jump-push buf)
                 (let [row (bound-range (dec (Integer. row)) 0 (-> buf :linescnt dec))]
                   (move-to-line buf row)))))

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
                  (and
                    (string? k)
                    (zero? (.indexOf k s))))
                (keys cmds)))]
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

(defn init-ex-mode-keymap[motion-keymap line-editor-keymap]
  (let [cmds (ex-commands motion-keymap)]
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
