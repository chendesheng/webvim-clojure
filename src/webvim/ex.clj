(ns webvim.ex
    (:require [me.raynes.fs :as fs]
              [clojure.core.async :as async]
              [clojure.java.io :as io]
              [snipsnap.core :as clipboard])
  (:use clojure.pprint
        (clojure [string :only (join blank?)])
        webvim.buffer
        webvim.cursor
        webvim.serve
        webvim.jumplist
        webvim.global)) 

(declare ex-commands)

(defn move-to-line[b row]
  (-> b 
      (assoc-in [:cursor :row] row)
      (cursor-line-start)))

(defn find-buffer [buffers f]
  (reduce-kv 
    (fn [matches _ b]
      (let [indexes (fuzzy-match (b :name) f)]
        (if (empty? indexes)
          matches
          (conj matches b)))) 
    [] buffers))

(defn change-active-buffer[id]
  (swap! registers assoc "#" @active-buffer-id)
  (reset! active-buffer-id id)
  (swap! registers assoc "%" id))

(defn execute [b]
  (let [[_ excmd args] (re-find #"\s*:([^\s]+)\s*(.*)\s*" (:ex b))]
    (if (nil? excmd)
      b
      (let [handlers (filter fn?
                             (map (fn[[cmd handler]]
                                    ;(println cmd)
                                    (if (string? cmd)
                                      (if (zero? (.indexOf cmd excmd)) handler nil)
                                      (let [m (re-find cmd excmd)]
                                        (if (not (nil? m)) handler nil)))) ex-commands))]
        (println handlers)
        (if (>= (count handlers) 1)
          ((first handlers) b excmd args)
          (assoc b :message "unknown command"))))))

(defn ex-tab-complete [ex]
  (if (re-test #"^:\S+\s*$" ex)
    (first 
      (filter 
        (fn[k]
          (zero? (.indexOf k ex)))
        (map 
          #(str ":" (first %)) 
          (filter #(-> % first string?) ex-commands)))) ex))

(defn set-ex-mode[b]
  (merge b {:mode ex-mode :ex ":" :message nil :keys nil}))

(defn set-ex-search-mode[b keycode]
  (println "set-ex-search-mode")
  (-> b 
      (merge {:ex keycode :message nil :keys nil})
      (assoc-in [:context :lastbuf] b)))

(def ex-commands
  (array-map 
    "write" (fn[b _ file]
              (if (not (blank? file))
                (-> b 
                    (assoc :name (fs/base-name file)) 
                    (assoc :filepath file) 
                    write-buffer)
                (if (nil? (b :filepath))
                  (assoc b :message "No file name")
                  (write-buffer b))))
   "nohlsearch" (fn[b _ _]
                  (dissoc b :highlights))
   "edit" (fn[b excmd file]
            (if (or (empty? file) (= file (:filepath b)))
              b ;TODO maybe we should refresh something when reopen same file?
              (let [newid (-> file new-file buf-info :id)]
                (change-active-buffer newid)
                (jump-push b)
                b)))
   "buffer" (fn [b execmd file]
              (let [matches (find-buffer @buffer-list file)
                    cnt (count matches)
                    equals (filter #(= (% :name) file) matches)]
                (cond 
                  (= (count equals) 1)
                  (let[id (-> equals first :id)]
                    (change-active-buffer id)
                    (if (not (= id (b :id)))
                      (jump-push b))
                    b)
                  (= 0 (count matches))
                  (assoc b :message "No file match")
                  (= 1 (count matches))
                  (let[id (-> matches first :id)]
                    (change-active-buffer id)
                    (if (not (= id (b :id)))
                      (jump-push b))
                    b)
                  (> (count matches) 1)
                  ;display matched buffers at most 5 buffers
                  (assoc b :message (str "which one? " (join ", " (map :name (take 5 matches))))))))
   "bnext" (fn[b execmd args]
             (let [id (b :id)
                   nextid (or
                            ;get next id larger than current
                            (->> @buffer-list   (map #(-> % last :id)) (filter #(> % id)) sort first)
                            (-> @buffer-list first last :id))]
               (println "nextid:" nextid)
               (if (not (= nextid id))
                 (do
                   (change-active-buffer nextid)
                   (jump-push b)))
               b))
   "bprev" (fn[b execmd args]
             (let [id (b :id)
                   nextid (or
                            (->> @buffer-list   (map #(-> % last :id)) (filter #(> % id)) sort first)
                            (-> @buffer-list first last :id))]
               (if (not (= nextid id))
                 (do
                   (change-active-buffer nextid)
                   (jump-push b)))
               b))
   "bdelete" (fn[b execmd args]
               (swap! buffer-list dissoc (b :id))
               (let [lastid (@registers "#")
                     nextid (if (nil? lastid)
                              (-> nil new-file :id)
                              lastid)]
                 (reset! active-buffer-id nextid)
                 (swap! registers assoc "%" nextid)
                 (swap! registers assoc "#" (-> @buffer-list first :id)))
               b)
   "eval" (fn[b execmd args]
            (->> args
                 read-string
                 eval
                 str
                 (assoc b :message)))
   #"^(\d+)$" (fn[b row _]
                (println "row:" row)
                (jump-push b)
                (let [row (bound-range (dec (Integer. row)) 0 (-> b :lines count dec))]
                  (move-to-line b row)))))

(defn ex-mode-default[b keycode]
  (let [ex (:ex b)
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
      (let [lb (-> b :context :lastbuf)
            newb (-> lb
                     (assoc :ex newex)
                     (save-lastbuf ""))] ;keep lastbuf avaiable on stack
        (try (cursor-next-str newb (subs newex 1))
             (catch Exception e newb)))
      (= \? (first newex))
      (let [lb (-> b :context :lastbuf)
            newb (-> lb
                     (assoc :ex newex)
                     (save-lastbuf ""))]
        (try (cursor-back-str newb (subs newex 1))
             (catch Exception e newb)))
      :else
      (assoc b :ex newex))))

