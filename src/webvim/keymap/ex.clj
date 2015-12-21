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
        webvim.core.parallel-universe
        webvim.core.ui
        webvim.jumplist
        webvim.core.utils
        webvim.core.event
        webvim.fuzzy
        webvim.keymap.external
        webvim.keymap.compile
        webvim.keymap.action))

(defn- set-line-buffer[buf s]
  (-> buf
      (assoc-in [:line-buffer :str] (rope s))
      (assoc-in [:line-buffer :pos] (count s))))

(defonce commands-history (atom (parallel-universe)))

(defn- save-history![buf]
  (let [s (-> buf :line-buffer :str str .trim)
        last-cmd (just-now @commands-history)]
    (if-not (or (empty? s) (= last-cmd s))
      (swap! commands-history 
             #(-> %
                  fast-forward
                  (new-future s))))
    buf))

(defn- recover-command-history[buf]
  (let [s (next-future @commands-history)]
    (if (nil? s) 
      buf
      (set-line-buffer buf s))))

(defn- find-buffer [buffers f]
  (reduce-kv
    (fn [matches _ abuf]
      (let [buf @abuf
            indexes (fuzzy-match (buf :name) f)]
        (if (empty? indexes)
          matches
          (conj matches buf))))
    [] buffers))

(defn- exec-shell-commands[buf cmds]
  (let [aoutputbuf (output-buf true)]
    (async/go
      (let [res (apply clojure.java.shell/sh cmds)
            s (if (empty? (res :out))
                (res :err) (res :out))]
        (write-output
          buf
          (str (string/join " " cmds) "\n" s)
          false)))
    (goto-buf buf aoutputbuf)))

(defn- expand-path[f]
  (if (= (first f) \~)
    (str (fs/expand-home f))
    (str (fs/normalized f))))

(defn cmd-write [buf _ file]
  (println (expand-path file))
  (if (not (string/blank? file))
    (-> buf
        (assoc :name (fs/base-name file))
        (assoc :filepath (expand-path file))
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
  (let [nextbuf @(or (get-buffer-from-reg (@registers "#")) (new-file nil))
        nextid (nextbuf :id)
        alternatebuf (some (fn[buf]
                             (if (not= (buf :id) nextid)
                               buf nil))
                           (map deref (vals @buffer-list)))]
    (registers-put! registers "%" (file-register nextbuf))
    (if (nil? alternatebuf)
      (registers-put! registers "#" nil)
      (registers-put! registers "#" (file-register alternatebuf)))
    (assoc buf :nextid nextid)))

(defn cmd-eval[buf execmd args]
  (try (->> args read-string eval str
            (assoc buf :message))
       (catch Exception e
              (assoc buf :message (str e)))))

(defn cmd-grep[buf execmd args]
  (exec-shell-commands buf ["grep" "-rnI" args "."]))

(defn cmd-find[buf execmd args]
  (exec-shell-commands buf ["find" "." "-name" args "-not" "-path" "*/.*"]))

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
                                (str (buf :id) ":" " " (printable-filepath buf))))
                            (vals @buffer-list)))) true))

(defn cmd-nohl[buf _ _] 
  (assoc buf :highlights []))

(defn cmd-edit[buf excmd file]
  (edit-file buf file true))

(defn cmd-history[buf _ _]
  (let [{before :before after :after} @commands-history
        all (concat (reverse before) after)]
    (write-output 
      buf
      (str ":history\n" (string/join "\n" all)) 
      true)))

(defn cmd-register[buf _ _]
  (write-output 
    buf
    (str ":register\n" 
         (string/join
           "\n" 
           (map (fn[reg]
                  (str "\"" (key reg)
                       "  "
                       (-> reg val :str)))
                @registers)))
    true))

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
         ["history" cmd-history]
         ["register" cmd-register]
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
          (-> buf
              ((first handlers) excmd args)
              save-history!)
          (assoc buf :message "unknown command"))))))

(defn- ex-tab-complete [{{r :str} :line-buffer :as buf} cmds]
  (let [[[_ subject]] (re-seq #"^e\s(.*)" r)]
    (println "ex-tab-complete:" subject)
    (if (nil? subject)
      (if (re-test #"^\s*\S+\s*$" r)
        (let [s (str r)
              news (first
                     (filter
                       (fn[k]
                         (and (string? k)
                              (.startsWith k s)))
                       (map first cmds)))]
          (if (nil? news) buf
            (update-in buf [:line-buffer]
                       (fn[linebuf]
                         (merge linebuf {:str (rope news) :pos (count news)})))))
        buf)
      (let [news (str "e " (-> buf :autocompl :suggestions second))]
        (println "news:" news)
        (update-in buf [:line-buffer]
                   (fn[linebuf]
                     (merge linebuf {:str (rope news)
                                     :pos (count news)})))))))

(defn- append-<br>[buf]
  (let [s (-> buf :line-buffer :str)
        len (count s)
        news (replacer s len len <br>)]
    (assoc-in buf [:line-buffer :str] news)))

(def ^:private all-files (atom nil))

(defn hidden?[f]
  ;(pprint (fs/split f))
  (or (fs/hidden? f)
      (not (not-any? #(re-test #"^\..+" %) (fs/split f)))))

(defn get-files[]
  (if (nil? @all-files)
    ;https://clojuredocs.org/clojure.core/tree-seq
    (let [dir? #(.isDirectory %)]
      (reset! all-files 
              (map (comp shorten-path str)
                   (filter #(not (or (dir? %)
                                     (hidden? %)))
                           (tree-seq (fn[f]
                                       (and (dir? f)
                                            (not (hidden? f)))) #(.listFiles %) fs/*cwd*)))))
    
    @all-files))

(defn- autocompl-suggest [subject targets]
  (take 10 
        (reduce #(conj %1 (last %2)) []
                (sort-by (juxt first second str)
                         (reduce 
                           (fn [suggestions word ] ;TODO sort by reference count?
                             (let [indexes (fuzzy-match word subject)]
                               (if (empty? indexes)
                                 suggestions
                                 (conj suggestions [(- (last indexes) 
                                                       (first indexes)) 
                                                    (first indexes) word])))) 
                           [[0 0 subject]] targets)))))

(defn- autocompl-path[buf]
  (let [[[_ subject]] (re-seq #"^e\s(.*)" (-> buf :line-buffer :str str))]
    (println "str:" (-> buf :line-buffer :str str))
    (println "subject:" subject)
    (println "autocompl:" (-> buf :autocompl))
    (if (empty? subject) buf
      (assoc-in buf [:autocompl :suggestions]
                (autocompl-suggest subject (get-files))))))

(defn init-ex-mode-keymap[line-editor-keymap]
  (let [cmds (ex-commands)]
    (merge line-editor-keymap
           {:enter (fn[buf keycode]
                     (println "ex-mode enter")
                     (swap! commands-history #(-> %
                                                  fast-forward
                                                  (assoc :current "")))
                     (dissoc buf :autocompl))
            :after (fn[buf keycode]
                     (let [after (or (line-editor-keymap :after) nop)
                           buf (after buf keycode)]
                       ;cache current typing content if it's latest one
                       (if (and (not (contains? #{"<c+p>" "<c+n>" "<cr>"} keycode))
                                (no-future? @commands-history))
                         (swap! commands-history assoc :current (-> buf :line-buffer :str str)))
                       (if (= keycode "<tab>")
                         buf
                         (autocompl-path buf))))
            :leave (fn[buf keycode]
                     (set-normal-mode buf))
            "<c+p>" (fn[buf]
                      (swap! commands-history go-back)
                      (let [buf (recover-command-history buf)]
                        buf))
            "<c+n>" (fn[buf]
                      (swap! commands-history go-future)
                      (if (no-future? @commands-history)
                        (set-line-buffer buf (@commands-history :current))
                        (recover-command-history buf)))
            "<cr>" (fn[buf]
                     (-> buf
                         ;append a <br> indicates this command is already executed
                         append-<br>
                         (execute cmds)))
            "<tab>" (fn[buf]
                      (println "<tab>")
                      (ex-tab-complete buf cmds))})))
