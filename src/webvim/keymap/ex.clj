(ns webvim.keymap.ex
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            [snipsnap.core :as clipboard])
  (:use clojure.pprint
        (clojure [string :only (join blank?)])
        webvim.core.rope
        webvim.core.line
        webvim.core.buffer
        webvim.core.serve
        webvim.core.register
        webvim.jumplist
        webvim.utils
        webvim.fuzzy
        webvim.keymap.action)) 

(defn- buf-info[buf]
  (if (and (empty? (buf :str))
           (not (fs/exists? (buf :filepath))))
    (assoc buf :message (str "[New File] " (buf :filepath)))
    (assoc buf :message (str "\"" (:filepath buf) "\""))))

(defn- move-to-line[buf row]
  (-> buf 
      (lines-row row)
      line-start))

(defn- find-buffer [buffers f]
  (reduce-kv 
    (fn [matches _ buf]
      (let [indexes (fuzzy-match (buf :name) f)]
        (if (empty? indexes)
          matches
          (conj matches buf)))) 
    [] buffers))

(def ex-commands
  (array-map 
    "write" (fn[buf _ file]
              (if (not (blank? file))
                (-> buf 
                    (assoc :name (fs/base-name file)) 
                    (assoc :filepath file) 
                    write-buffer)
                (if (nil? (buf :filepath))
                  (assoc buf :message "No file name")
                  (write-buffer buf))))
   "nohlsearch" (fn[buf _ _]
                  (dissoc buf :highlights))
   "edit" (fn[buf excmd file]
            (if (or (empty? file) (= file (:filepath buf)))
              buf
              (let [newid (-> file new-file buf-info :id)]
                (change-active-buffer newid)
                (jump-push buf)
                buf)))
   "buffer" (fn [buf execmd file]
              (let [matches (find-buffer @buffer-list file)
                    cnt (count matches)
                    equals (filter #(= (% :name) file) matches)]
                (cond 
                  (= (count equals) 1)
                  (let[id (-> equals first :id)]
                    (change-active-buffer id)
                    (if (not (= id (buf :id)))
                      (jump-push buf))
                    buf)
                  (= 0 (count matches))
                  (assoc buf :message "No file match")
                  (= 1 (count matches))
                  (let[id (-> matches first :id)]
                    (change-active-buffer id)
                    (if (not (= id (buf :id)))
                      (jump-push buf))
                    buf)
                  (> (count matches) 1)
                  ;display matched buffers at most 5 buffers
                  (assoc buf :message (str "which one? " (join ", " (map :name (take 5 matches))))))))
   "bnext" (fn[buf execmd args]
             (let [id (buf :id)
                   nextid (or
                            ;get next id larger than current
                            (->> @buffer-list   (map #(-> % last :id)) (filter #(> % id)) sort first)
                            (-> @buffer-list first last :id))]
               ;(println "nextid:" nextid)
               (if (not (= nextid id))
                 (do
                   (change-active-buffer nextid)
                   (jump-push buf)))
               buf))
   "bprev" (fn[buf execmd args]
             (let [id (buf :id)
                   nextid (or
                            (->> @buffer-list   (map #(-> % last :id)) (filter #(> % id)) sort first)
                            (-> @buffer-list first last :id))]
               (if (not (= nextid id))
                 (do
                   (change-active-buffer nextid)
                   (jump-push buf)))
               buf))
   "bdelete" (fn[buf execmd args]
               (swap! buffer-list dissoc (buf :id))
               (let [lastbuf (@registers "#")
                     nextbuf (or lastbuf
                               (new-file nil))
                     firstbuf (first @buffer-list)]
                 (reset! active-buffer-id (nextbuf :id))
                 (swap! registers assoc "%" {:id (nextbuf :id) :str (nextbuf :str)})
                 (if (or (nil? firstbuf) (= (firstbuf :id) (nextbuf :id)))
                   (swap! registers assoc "#" nil)
                   (swap! registers assoc "#" {:id (firstbuf :id) :str (firstbuf :str)})))
               buf)
   "eval" (fn[buf execmd args]
            (->> args
                 read-string
                 eval
                 str
                 (assoc buf :message)))
   #"^(\d+)$" (fn[buf row _]
                ;(println "row:" row)
                (jump-push buf)
                (let [row (bound-range (dec (Integer. row)) 0 (-> buf :linescnt dec))]
                  (move-to-line buf row)))))

(defn- execute [buf]
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
                                        (if (not (nil? m)) handler nil)))) ex-commands))]
        (println excmd args)
        (if (>= (count handlers) 1)
          ((first handlers) buf excmd args)
          (assoc buf :message "unknown command"))))))

(defn- ex-tab-complete [{{r :str} :line-buffer :as buf}]
  (if (re-test #"^\s*\S+\s*$" r)
    (let [s (str r)
          news (first 
              (filter 
                (fn[k]
                  (and
                    (string? k)
                    (zero? (.indexOf k s))))
                (keys ex-commands)))]
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
  (merge line-editor-keymap 
         {:enter (fn[buf keycode]
                   (-> buf
                       ((line-editor-keymap :enter) keycode)
                       (assoc :mode ex-mode)))
          "<cr>" (fn[buf]
                   (-> buf
                       ;append a <br> indicates this command is already executed
                       append-<br>
                       execute))
          "<tab>" ex-tab-complete}))
