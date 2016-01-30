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
        webvim.exec
        webvim.keymap.external
        webvim.keymap.linebuf.linebuf
        webvim.keymap.compile
        webvim.keymap.action))

;cache
;TODO: monitor disk file change
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
                   (filter (comp not hidden?)
                           (tree-seq (fn[f]
                                       (and (dir? f)
                                            (not (hidden? f)))) #(.listFiles %) fs/*cwd*)))))
    
    @all-files))

(defn- set-line-buffer[buf s]
  (-> buf
      (assoc-in [:line-buffer :str] (rope s))
      (assoc-in [:line-buffer :pos] (count s))))

(defn- find-buffer [buffers f]
  (reduce-kv
    (fn [matches _ abuf]
      (let [buf @abuf
            indexes (fuzzy-match (buf :name) f)]
        (if (empty? indexes)
          matches
          (conj matches buf))))
    [] buffers))

(defn- exec-shell-commands[buf panel cmds]
  (exec-async cmds (fn[line]
                     (append-panel buf panel line false)))
  (append-panel buf panel (str (string/join " " cmds)) true))

(defn- expand-path[f]
  (if (= (first f) \~)
    (str (fs/expand-home f))
    (str (fs/normalized f))))

(defn cmd-write [buf _ file]
  ;(println (expand-path file))
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
  (let [nextbuf @(or (get-buffer-from-reg (registers-get "#")) (new-file nil))
        nextid (nextbuf :id)
        alternatebuf (some (fn[buf]
                             (if (not= (buf :id) nextid)
                               buf nil))
                           (map deref (vals @buffer-list)))]
    (registers-put! "%" (file-register nextbuf))
    (if (nil? alternatebuf)
      (registers-put! "#" nil)
      (registers-put! "#" (file-register alternatebuf)))
    (assoc buf :nextid nextid)))

(defn cmd-eval[buf execmd args]
  (try 
    (let [result (atom nil)
          output (with-out-str 
                   (->> args read-string eval str
                        (reset! result)))]
      (append-output-panel
        buf
        (str 
          ":" execmd " " args
          output \newline
          @result \newline)
        true))
    (catch Exception e
      (assoc buf :message (str e)))))

(defn cmd-grep[buf execmd args]
  (exec-shell-commands buf (grep-panel) ["grep" "-rnI" args "."]))

(defn cmd-find[buf execmd args]
  (exec-shell-commands buf (find-panel) ["find" "." "-name" args "-not" "-path" "*/.*"]))

(defn cmd-move-to-line[buf row _]
;(println "row:" row)
  (jump-push buf)
  (let [row (bound-range (dec (Integer. row)) 0 (-> buf :linescnt dec))]
    (move-to-line buf row)))

(defn cmd-cd[buf execmd args]
  (if (string/blank? args) (assoc buf :message (str fs/*cwd*))
    (let [dir (fs/expand-home args)]
      ;(println "dir:" fs/*cwd*)
      ;(println "dir:2" dir)
      (if (fs/directory? dir)
        (do
          (reset! all-files nil)
          (alter-var-root (var fs/*cwd*) (constantly dir))
          (assoc buf :message (str "Set working directory to: " fs/*cwd*)))
        (assoc buf :message "Path is not a directory or not exists.")))))

(defn cmd-ls[buf execmd args]
  (append-output-panel buf
                (str ":ls\n"
                     (string/join 
                       "\n" 
                       (map (fn[abuf]
                              (let [buf @abuf]
                                (str (buf :id) ":" " " (printable-filepath buf))))
                            (vals @buffer-list))) "\n") true))

(defn cmd-nohl[buf _ _] 
  (assoc buf :highlights []))

(defn cmd-edit[buf excmd file]
  (edit-file buf file true))

(def ^:private commands-history (atom (parallel-universe)))

(defn cmd-history[buf _ _]
  (let [{before :before after :after} @commands-history
        all (concat (reverse before) after)]
    (append-output-panel 
      buf
      (str ":history\n" (string/join "\n" all) "\n") 
      true)))

(defn cmd-register[buf _ _]
  (append-output-panel 
    buf
    (str ":register\n" 
         (string/join
           "\n" 
           (map-registers (fn[[k v]]
                            (str "\"" k
                                 "  "
                                 (clojure.string/escape (or (:str v) "") {\newline "\\n"}))))) "\n")
    true))

(defn cmd-jumps[buf _ _]
  (append-output-panel 
    buf
    (str ":jumps\n" 
         (string/join
           "\n" 
           (map (fn[item]
                  (format "%s:%s" (or (item :filepath) (item :name)) (item :y)))
                (@jump-list :before))) "\n")
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
         ["jumps" cmd-jumps]
         ["cd" cmd-cd]
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
        ;(println excmd args)
        (if (>= (count handlers) 1)
          ((first handlers) buf excmd args)
          (-> buf
              (assoc :message "unknown command")
              (dissoc :line-buffer)))))))

(defn- replace-suggestion[buf w _]
  (let [news (str "e " w)]
        (update-in buf [:line-buffer]
                   (fn[linebuf]
                     (merge linebuf {:str (rope news)
                                     :pos (count news)})))))

(defn- uncomplete-word[{{r :str} :line-buffer :as buf}]
  (let [[[_ w]] (re-seq #"^e\s(.*)" (-> buf :line-buffer :str str))] w))

(defn- new-autocompl[buf]
  (if (-> buf :autocompl nil?) 
    (assoc buf :autocompl
           {:words (get-files)
            :suggestions nil
            :index 0
            :uncomplete-word uncomplete-word
            :replace replace-suggestion
            :limit-number 20})
    buf))

(defn- ex-tab-complete [{{r :str} :line-buffer :as buf} cmds]
  (let [w (uncomplete-word buf)]
    ;(println "ex-tab-complete:" w)
    (if (nil? w)
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
                         (assoc linebuf :str (rope news) :pos (count news))))))
        buf)
      (autocompl-move 
        (new-autocompl buf) inc))))

;Make sure each cmd have a not-nil response message
(defn init-ex-mode-keymap[]
  (let [cmds (ex-commands)
        linebuf-keymap (init-linebuf-keymap commands-history)
        enter (or (linebuf-keymap :enter) nop)
        after (or (linebuf-keymap :after) nop)
        leave (or (linebuf-keymap :leave) nop)]
    ;TODO: consider add an "inherit" (or "wrap") function
    (assoc linebuf-keymap
           :enter (fn[buf keycode]
                    (-> buf
                        (enter keycode)
                        (assoc :autocompl nil)))
           :after (fn[buf keycode]
                    (let [buf (after buf keycode)]
                      (if (or (= keycode "<tab>")
                              (= keycode "<s-tab>")
                              (-> buf :autocompl nil?))
                        buf
                        (autocompl-suggest buf))))
           :leave (fn[buf keycode]
                      (-> buf
                          (leave keycode)
                          (assoc :autocompl nil)
                          set-normal-mode))
           "<cr>" (fn[buf]
                    (execute buf cmds))
           "<s-tab>" (fn[buf]
                       (autocompl-move buf dec))
           "<tab>" (fn[buf]
                     (ex-tab-complete buf cmds)))))


