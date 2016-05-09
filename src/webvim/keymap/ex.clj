(ns webvim.keymap.ex
  (:require [me.raynes.fs :as fs]
            [webvim.mode :refer [set-normal-mode]]
            [webvim.core.editor :refer [update-cwd]]
            [webvim.panel :refer [append-panel append-output-panel grep-panel ag-panel find-panel edit-file goto-buf]]
            [clojure.string :as string]
            [webvim.core.eval :refer [eval-refer-ns]])
  (:use clojure.pprint
        webvim.core.rope
        webvim.core.line
        webvim.core.buffer
        webvim.core.register
        webvim.core.diff
        webvim.core.pos
        webvim.core.parallel-universe
        webvim.core.ui
        webvim.jumplist
        webvim.core.utils
        webvim.core.event
        webvim.fuzzy
        webvim.exec
        webvim.keymap.linebuf.linebuf
        webvim.keymap.compile))

(defn- hidden? [f]
  ;(pprint (fs/split f))
  (or (fs/hidden? f)
      (some #(re-test #"^\..+" %) (fs/split f))))

(defn- file-seq-bfs
  ([pred dirs]
    (if (empty? dirs)
      nil
      (let [[dir & dirs] dirs
            files (filter pred (.listFiles dir))
            more-dirs (filter fs/directory? files)]
        (concat files
                (lazy-seq
                  (file-seq-bfs pred
                                (concat dirs more-dirs)))))))
  ([pred]
    (file-seq-bfs pred (list fs/*cwd*))))

(defn- get-files []
  (map (comp (fn [f] {:name f :class (cond
                                       (fs/directory? f) "dir"
                                       (fs/executable? f) "exe"
                                       :else "file")})
             (fn [s]
               (shorten-path s))
             str) (file-seq-bfs (comp not hidden?)))) 

;(pprint (take 20 (get-files)))

(defn- set-line-buffer [buf s]
  (-> buf
      (assoc-in [:line-buffer :str] (rope s))
      (assoc-in [:line-buffer :pos] (count s))))

(defn- find-buffer [buffers f]
  (reduce
    (fn [matches buf]
      (let [indexes (fuzzy-match (buf :name) f)]
        (if (empty? indexes)
          matches
          (conj matches buf))))
    [] buffers))

(defn- unquote-args [args]
  (map (fn [s]
         (if (and (.startsWith s "\"")
                  (.endsWith s "\""))
           (subs s 1 (-> s count (- 2)))
           s)) args))

(defn- exec-shell-commands [buf apanel cmds]
  (println "exec-shell-commands:" cmds)
  (exec-async (-> cmds flatten unquote-args)
              (fn [line]
                (append-panel buf apanel (str line \newline) false)))
  (append-panel buf apanel (str \newline (string/join \space (flatten cmds)) \newline) true))

(defn cmd-write [buf _ _ [file]]
  (if (not (string/blank? file))
    (-> buf
        (assoc :name (fs/base-name file))
        (assoc :filepath (expand-path file))
        write-buffer)
    (if (nil? (buf :filepath))
      (assoc buf :message "No file name")
      (write-buffer buf))))

(defn cmd-buffer [buf _ _ [file]]
  (let [matches (find-buffer (get-buffers) file)
        cnt (count matches)
        equals (filter #(= (% :name) file) matches)]
    (cond
      (= (count equals) 1)
      (goto-buf buf (-> equals first :id))
      (= 0 cnt)
      (assoc buf :message "No file match")
      (= 1 cnt)
      (goto-buf buf (-> matches first :id))
      (> cnt 1)
      ;display matched buffers at most 5 buffers
      (assoc buf :message (str "which one? " (string/join ", " (map :name (take 5 matches))))))))

(defn- get-buffers-id []
  (map :id (get-buffers)))

(defn- empty-nil [f]
  (fn [& args]
    (if (empty? args)
      nil
      (apply f args))))

(defn cmd-bnext [buf _ _ _]
  (let [id (buf :id)
        nextid (or
                 ;get next id larger than current
                 (->> (get-buffers-id) (filter #(> % id)) (apply (empty-nil min)))
                 (apply min (get-buffers-id)))]
    (goto-buf buf nextid)))

(defn cmd-bprev [buf _ _ _]
  (let [id (buf :id)
        nextid (or
                 (->> (get-buffers-id) (filter #(< % id)) (apply (empty-nil max)))
                 (apply max (get-buffers-id)))]
    (goto-buf buf nextid)))

(defn- get-buffer-from-reg [reg]
  (if (nil? reg) nil
      (get-buffer-agent (reg :id))))

(defn cmd-bdelete [buf _ _ _]
  (remove-buffer (buf :id))
  (let [nextbuf @(or (get-buffer-from-reg (registers-get "#")) (new-file nil))
        nextid (nextbuf :id)
        alternatebuf (some (fn [buf]
                             (if (not= (buf :id) nextid)
                               buf nil))
                           (get-buffers))]
    (registers-put! "%" (file-register nextbuf))
    (if (nil? alternatebuf)
      (registers-put! "#" nil)
      (registers-put! "#" (file-register alternatebuf)))
    (-> buf
        (assoc :nextid nextid)
        (fire-event :close-buffer))))

(defn- eval-sep [content]
  (str (repeat-chars 80 \=) \newline
       content \newline))

(defn print-eval [buf code]
  (let [{output :output
         exception :exception} (eval-refer-ns nil code)]
    (if (nil? exception)
      (append-output-panel 
        buf
        (eval-sep (format ":%s\n %s" code output))
        true)
      (assoc buf :message (str exception)))))


(defn do-eval [buf code]
  (let [{result :result
         output :output
         exception :exception} (eval-refer-ns (get-namespace (buf :filepath)) code)]
    (if (nil? exception)
      (let [lines (apply concat
                         (map string/split-lines
                              (filter (comp not empty?)
                                      (list output result))))
            cnt (count lines)]
        (if (zero? cnt)
          (assoc buf :message "no result and output")
          (append-output-panel
            (update buf :message #(if (= cnt 1) (first lines) %))
            (eval-sep (string/join \newline (conj lines (str ":" code))))
            (> cnt 1))))
      (assoc buf :message (str exception)))))

(defn cmd-eval [buf _ [la lb] _]
  (let [[a _] (line-range buf la)
        [_ b] (line-range buf lb)
        code (subr (buf :str) a b)]
    ;(println "code:" code)
    (do-eval buf code)))

(defn cmd-grep [buf _ _ args]
  (exec-shell-commands buf (grep-panel)
                       ["grep" "-rnI" args "."]))

(defn cmd-ag [buf _ _ args]
  (exec-shell-commands buf (ag-panel)
                       ["ag" "--vimgrep" args]))

(defn cmd-find [buf _ _ args]
  (exec-shell-commands buf (find-panel)
                       ["find" "." "-name" args "-not" "-path" "*/.*"]))

(defn cmd-move-to-line [buf cmd [row]]
;(println "row:" row)
  (jump-push buf)
  (let [row (bound-range (dec (Integer. row)) 0 (-> buf buf-total-lines dec))]
    (move-to-line buf row)))

(defn cmd-cd [buf _ _ [path]]
  (if (string/blank? path) (assoc buf :message (str fs/*cwd*))
      (let [dir (expand-path path)]
        (if (fs/directory? dir)
          (assoc buf :message (str "Change working directory to: " (update-cwd dir)))
          (assoc buf :message "Path is not a directory or not exists.")))))

(defn cmd-git [buf _ _ args]
  (let [res (apply clojure.java.shell/sh (concat ["git"] args [:dir (str fs/*cwd*)]))]
    (append-output-panel buf (res :out) true)))

(defn cmd-ls [buf _ _ _]
  (append-output-panel buf
                       (str ":ls\n"
                            (string/join 
                              "\n" 
                              (map (fn [buf]
                                     (format "%d: %s" (buf :id) (printable-filepath buf)))
                                   (sort-by :id (get-buffers)))) "\n") true))

(defn cmd-nohl [buf _ _ _] 
  (assoc buf :highlights []))

(defn- buf-reload [buf]
  (let [f (buf :filepath)
        res (clojure.java.shell/sh "diff" "-" f "-u" :in (-> buf :str str))]
    (if (-> res :err empty?) 
      (let [changes (time (parse-diff (str (res :out))))]
        (-> buf
            (apply-line-changes changes)
            save-undo
            set-save-point 
            set-mod-time
            (assoc :message (format "File reloaded, %d change(s)" (count changes)))))
      (assoc buf :message (res :err)))))

(defn cmd-diff [buf _ _ _]
  (let [f (buf :filepath)
        ;TODO: use Java diff library instead of shell command
        res (clojure.java.shell/sh "diff" "-" f " -u" :in (-> buf :str str))]
    (if (-> res :err empty?) 
      (let [diff (-> res :out str)]
        (if (string/blank? diff)
          (assoc buf :message "no changes")
          (append-output-panel buf
                               (str "diff - " f "-u" "\n" diff) true)))
      (assoc buf :message (res :err)))))

(defn cmd-edit [buf _ _ args]
  (let [[file linenum] args]
    (println "edit:" file linenum (count args))
    (if (nil? linenum)
      (if (and (empty? file) (fs/exists? (buf :filepath)))
        (buf-reload buf)
        (edit-file buf file true))
      (edit-file buf file (parse-int linenum) true))))

(defonce ^:private commands-history (atom (parallel-universe)))

(defn cmd-history [buf _ _ _]
  (let [{before :before after :after} @commands-history
        all (concat (reverse before) after)]
    (append-output-panel 
      buf
      (str ":history\n" (string/join "\n" all) "\n") 
      true)))

(defn cmd-register [buf _ _ _]
  (append-output-panel 
    buf
    (str ":register\n" 
         (string/join
           "\n" 
           (map-registers (fn [[k v]]
                            (str "\"" k
                                 "  "
                                 (clojure.string/escape (or (:str v) "") {\newline "\\n"}))))) "\n")
    true))

(defn cmd-jumps [buf _ _ _]
  (append-output-panel 
    buf
    (str ":jumps\n" 
         (string/join
           "\n" 
           (map (fn [item]
                  (format "%s:%s" (or (item :filepath) (item :name)) (item :y)))
                (jumplist-before))) "\n")
    true))

;TODO: ex command parser
(defn- ex-commands [buf]
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
         ["ag" cmd-ag]
         ["find" cmd-find]
         ["history" cmd-history]
         ["register" cmd-register]
         ["jumps" cmd-jumps]
         ["cd" cmd-cd]
         ["ls" cmd-ls]
         ["git" cmd-git]
         ["diff" cmd-diff]]]
    (fire-event :init-ex-commands cmds buf)))

;About ctx:
; {:ranges []
;  :cmd ""
;  :args ""}
(defn split-ex-cmd [s]
  (println "split-excmd" s)
  (reduce
    (fn [ctx [item _]]
      (if (-> ctx :cmd nil?)
        (if (re-test #"^[a-zA-Z~<>@=#*&!\(]" item)
          (assoc ctx :cmd item)
          (if (= item "%")
            (update ctx :ranges vconj "1" "," "$")
            (update ctx :ranges vconj item)))
        (assoc ctx :args item)))
    {}
    (re-seq #"[.$%,;]|[+-]?\d+|'[a-zA-Z0-9<>{}\[\]()']|/(\[(\\\\|\\\]|/|[^\]])*\]|\\\\|\\/|[^/\[\]])+/?|[a-zA-Z~<>@=#*&!\(][^\s]*|.+" s)))

(defn split-arguments [s]
  (map first (re-seq #"\"(\\\\|\\\"|[^\"]*)\"|[^\s]+" (or s ""))))

(defn- parse-mark [buf ch]
  (linenum-by-pos
    buf
    (cond
      (= ch \<) (-> buf :visual :range sort2 first)
      (= ch \>) (-> buf :visual :range sort2 last)
      :else (throw
              (str "unknown mark:" ch)))))

(defn- parse-range [ranges dot $ buf]
  (letfn [(calc-delta [delta op rg]
            (op (or delta 0) (Integer. (subs rg 1))))
          (return-nil-if-all-values-nil [coll]
                                        (if (every? #(-> coll % nil?) (keys coll))
                                          nil coll))
          (next-res [{base :base delta :delta} rg]
                    (return-nil-if-all-values-nil
                      {:base (cond
                               (re-test #"^\d" rg) (-> rg Integer. dec)
                               (= "$" rg) $
                               (= "." rg) dot
                               ;TODO: (.startsWith "/" rg)
                               (-> rg first (= \'))
                               (parse-mark buf (last rg))
                               :else base)
                       :delta (if (re-test #"^[+-]\d" rg)
                                (+ (or delta 0) (Integer. rg))
                                delta)}))
          (res-start [res]
                     (or (:end res) (:start res)))]
    (loop [[rg & restrg] (map #(.trim %) ranges)
           state :start
           res nil]
      (cond
        (nil? rg)
        (let [start (+ (-> res :start :base (or dot))
                       (-> res :start :delta (or 0)))]
          (sort2
            [start
             (if (-> res :end :base nil?)
               start
               (+ (-> res :end :base)
                  (-> res :end :delta (or 0))))]))
        (= rg ",")
        (recur restrg :end {:start (res-start res)})
        (= rg ";")
        ;FIXME: doesn't feel right
        (recur restrg :end {:start (res-start res)
                            :end (-> res
                                     res-start
                                     (dissoc :delta))})
        :else
        (recur restrg state (update res state next-res rg))))))

(defn parse-excmd [buf s]
  (let [{ranges :ranges cmd :cmd args :args} (split-ex-cmd s)]
    {:range (parse-range ranges (buf :y) (buf-total-lines buf) buf)
     :cmd cmd
     :args (split-arguments args)}))


(comment
  (parse-excmd "1,./^haha\\\\[\\\\\\]/]hello\\//+1grep hello")
  (parse-excmd "%grep hello")
  (parse-excmd ",-2$-1grep hello")
  (parse-excmd "2%grep hello")
  (parse-excmd "2,%grep hello")
  (parse-excmd "2,-2+2-3+4$grep hello")
  (parse-excmd "2+1,1,2grep hello"))


(defn- execute [buf cmds]
  (let [s (-> buf :line-buffer :str str .trim)]
    (if (-> s first (= \())
      (do-eval buf s)
      (let [{excmd :cmd rg :range args :args} (parse-excmd buf s)]
        (pprint [excmd rg args])
        (if (nil? excmd)
          (if (empty? rg)
            buf
            (-> buf
                jump-push
                (move-to-line (last rg))))
          (let [handlers (filter seq?
                                 (map (fn [[cmd handler]]
                                        (if (.startsWith cmd excmd)
                                          (list handler cmd args))) cmds))]
            ;(println "handlers:")
            ;(pprint handlers)
            (if (>= (count handlers) 1)
              (let [[[handler cmd args]] handlers
                    buf (handler buf cmd rg args)]
                (registers-put! ":" {:str s})
                buf)
              (-> buf
                  (assoc :message "!!!unknown command!!!")
                  (assoc :beep true)
                  (dissoc :line-buffer)))))))))

(defn- ex-tab-complete [{{r :str} :line-buffer :as buf} cmds]
  (if (re-test #"^\s*\S+\s*$" r)
    (let [s (str r)
          news (first
                 (filter
                   (fn [k]
                     (and (string? k)
                          (.startsWith k s)))
                   (map first cmds)))]
      (if (nil? news) buf
          (update buf :line-buffer
                  (fn [linebuf]
                    (assoc linebuf :str (rope news) :pos (count news))))))
    buf))

(defn- ex-replace-suggestion [buf w]
  (let [news (str "e " w)]
    (update buf :line-buffer
            (fn [linebuf]
              (merge linebuf {:str (rope news)
                              :pos (count news)})))))

(defn- ex-uncomplete-word [{{r :str} :line-buffer :as buf}]
  (let [[[_ w]] (re-seq #"^e\s(\S+)" (-> buf :line-buffer :str str))] w))

(def ex-autocompl-provider
  {:move-up "<s-tab>"
   :move-down "<tab>"
   :uncomplete-word ex-uncomplete-word
   :replace-suggestion (fn [buf w _]
                         (ex-replace-suggestion buf (w :name)))
   :async true
   :fn-words (fn [buf w] (get-files))
   :fn-suggest fuzzy-suggest
   :limit-number 20
   :start-autocompl? (fn [buf keycode]
                       (if (-> buf :line-buffer nil?)
                         buf
                         (->> buf
                              :line-buffer
                              :str
                              (re-test #"^e\s\S+"))))
   :continue-autocompl? (fn [_ _] true)})

(defn init-ex-mode-keymap [buf]
  (let [cmds (ex-commands buf)
        ex-mode-keymap
        (-> (init-linebuf-keymap commands-history)
            (wrap-key :enter
                      (fn [handler]
                        (fn [buf keycode]
                          (-> buf
                              (assoc :mode :ex-mode)
                              (handler keycode)))))
            (wrap-key :leave
                      (fn [handler]
                        (fn [buf keycode]
                          (-> buf
                              (handler keycode)
                              set-normal-mode))))
            (assoc "<cr>"
                   (fn [buf keycode]
                     (execute buf cmds))
                   "<tab>"
                   (fn [buf keycode]
                     (ex-tab-complete buf cmds))))]
    (fire-event :ex-mode-keymap ex-mode-keymap buf)))

(listen :normal-mode-keymap
        (fn [keymap buf]
          (assoc keymap
                 ":" (init-ex-mode-keymap buf))))

(listen :visual-mode-keymap
        (fn [keymap buf]
          (assoc keymap
                 ":" (wrap-key (init-ex-mode-keymap buf)
                               :enter
                               (fn [handler]
                                 (fn [buf keycode]
                                   (-> buf
                                       (assoc :line-buffer {:prefix keycode
                                                            :str (rope "'<,'>")
                                                            :pos 5})
                                       (handler keycode))))))))

(defn wrap-command [cmds cmd f]
  (map
    (fn [[key fncmd :as item]]
      (if (= key cmd)
        [key (f fncmd)]
        item)) cmds))

(listen :switch-buffer
        (fn [buf]
          (if (and (not= (buf :mod-time) (mod-time buf))
                   (not (buf :dirty)))
            (buf-reload buf)

            buf)))
