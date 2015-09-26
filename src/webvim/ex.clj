(ns webvim.ex
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
        webvim.register
        webvim.action.motion
        webvim.action.edit
        webvim.action.mode
        webvim.action.highlight
        webvim.jumplist
        webvim.utils
        webvim.fuzzy)) 

(declare ex-commands)

(defn buf-info[buf]
  (if (and (empty? (buf :str))
           (not (fs/exists? (buf :filepath))))
    (assoc buf :message (str "[New File] " (buf :filepath)))
    (assoc buf :message (str "\"" (:filepath buf) "\""))))

(defn move-to-line[buf row]
  (-> buf 
      (lines-row row)
      line-start))

(defn find-buffer [buffers f]
  (reduce-kv 
    (fn [matches _ buf]
      (let [indexes (fuzzy-match (buf :name) f)]
        (if (empty? indexes)
          matches
          (conj matches buf)))) 
    [] buffers))

(defn execute [buf]
  (let [[_ excmd args] (re-find #"\s*:([^\s]+)\s*(.*)\s*" (:ex buf))]
    (if (nil? excmd)
      buf
      (let [handlers (filter fn?
                             (map (fn[[cmd handler]]
                                    ;(println cmd)
                                    (if (string? cmd)
                                      (if (zero? (.indexOf cmd excmd)) handler nil)
                                      (let [m (re-find cmd excmd)]
                                        (if (not (nil? m)) handler nil)))) ex-commands))]
        ;(println handlers)
        (if (>= (count handlers) 1)
          ((first handlers) buf excmd args)
          (assoc buf :message "unknown command"))))))

(defn ex-tab-complete [ex]
  (if (re-test #"^:\S+\s*$" ex)
    (first 
      (filter 
        (fn[k]
          (zero? (.indexOf k ex)))
        (map 
          #(str ":" (first %)) 
          (filter #(-> % first string?) ex-commands)))) ex))

(defn set-ex-mode[buf]
  (merge buf {:mode ex-mode :ex ":" :message nil :keys nil}))

(defn set-ex-search-mode[buf keycode]
  (-> buf 
      (merge {:ex keycode :message nil :keys nil})
      (assoc-in [:context :lastbuf] buf)))

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
              buf ;TODO maybe we should refresh something when reopen same file?
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
               (let [lastid (@registers "#")
                     nextid (if (nil? lastid)
                              (-> nil new-file :id)
                              lastid)]
                 (reset! active-buffer-id nextid)
                 (swap! registers assoc "%" nextid)
                 (swap! registers assoc "#" (-> @buffer-list first :id)))
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

(defn ex-mode-default[buf keycode]
  (let [ex (:ex buf)
        newex (cond 
                (= keycode "space")
                (str ex " ")
                (= keycode "backspace")
                (subs ex 0 (-> ex count dec))
                (= keycode "tab")
                (ex-tab-complete ex)
                (= 1 (count keycode))
                (str ex keycode)
                :else ex)]
    (cond 
      (= \/ (first newex))
      (let [lb (-> buf :context :lastbuf)
            newb (-> lb
                     (assoc :ex newex)
                     (dissoc :highlights)
                     (save-lastbuf ""))] ;keep lastbuf avaiable on stack
        (try (re-forward-highlight newb (re-pattern (subs newex 1)))
             (catch Exception e newb)))
      (= \? (first newex))
      (let [lb (-> buf :context :lastbuf)
            newb (-> lb
                     (assoc :ex newex)
                     (dissoc :highlights)
                     (save-lastbuf ""))]
        (try (re-backward-highlight newb (re-pattern (subs newex 1)))
             (catch Exception e newb)))
      :else
      (assoc buf :ex newex))))

