;common actions
(ns webvim.keymap.action
  (:require [me.raynes.fs :as fs]
            [clojure.string :as string])
  (:use webvim.core.buffer
        webvim.core.rope
        webvim.core.line
        webvim.core.pos
        webvim.core.register
        webvim.core.event
        webvim.indent
        webvim.fuzzy
        webvim.core.lang
        webvim.core.utils
        webvim.jumplist
        webvim.keymap.compile
        webvim.core.ui
        clojure.pprint))

(def normal-mode 0)
(def insert-mode 1)
(def ex-mode 2)

;submodes
(def temp-normal-mode 1)

;0 means no visual
(def no-visual 0)
(def visual-range 1)
(def visual-line 2)
(def visual-block 3) ;TODO

(defn file-register [buf]
  {:id (buf :id) :str (or (buf :filepath) (buf :name))})

(defn- add-highlight [buf rg]
  (let [highlights (buf :highlights)]
    (if (empty? (filter (fn [[a b]]
                          (and (= a (rg 0)) (= b (rg 1)))) highlights))
      (update-in buf [:highlights] conj rg) buf)))

(defn re-forward-highlight [buf re]
  (let [pos (buf :pos)
        r (buf :str)
        rg (or
             (pos-re+ r (inc pos) re)
             (pos-re+ r 0 re))] ;TODO: use region reduce duplicate work
    (if (nil? rg) buf
        (let [[a b] rg]
          (-> buf
              (buf-set-pos a)
              (add-highlight [a (dec b)]))))))

(defn re-backward-highlight [buf re]
  (let [pos (buf :pos)
        r (buf :str)
        rg (or
             (pos-re- r (dec pos) re)
             (pos-re- r (-> r count dec) re))]
    (if (nil? rg) buf
        (let [[a b] rg]
          (-> buf
              (buf-set-pos a)
              (add-highlight [a (dec b)]))))))

(defn set-insert-mode [buf]
  (assoc buf
         :mode insert-mode
         :keymap (buf :insert-mode-keymap)))

(defn set-normal-mode [buf]
  ;(println "set-normal-mode:")
  (assoc buf
         :mode normal-mode
         :keymap (buf :normal-mode-keymap)))

(defn buf-yank
  ([buf a b linewise? delete?]
    (let [s (buf-subr buf a b)]
      ((if delete?
         registers-delete-to!
         registers-yank-to!) (-> buf :context :register) {:str s :linewise? linewise?})
      (update-in buf [:context] dissoc :register)))
  ([buf a b linewise?]
    (buf-yank buf a b linewise? false)))

(defn change-active-buffer [id nextid]
  (let [path-name #(or (% :filepath) (% :name))]
    (if (not= id nextid)
      (do
        (registers-put! "#" 
                        (file-register
                          (-> @buffer-list (get id) deref)))
        (registers-put! "%"
                        (file-register
                          (-> @buffer-list (get nextid) deref)))))))

;collect range argument, TODO: add linewise
(defn range-prefix [{{tp :type rg :range} :visual :as buf} inclusive?]
  (cond
    (= tp visual-range)
    (make-range rg inclusive?)
    (= tp visual-line)
    (make-linewise-range rg buf)
    (= tp visual-block)
    (throw (Exception. "TODO: visual-block"))
    (-> buf :context :range nil? not)
    (-> buf :context :range (make-range inclusive?))
    :else (throw (Exception. "no range prefix exist"))))

(defn change-range [buf inclusive? linewise?]
  (let [[a b] (range-prefix buf inclusive?)]
    (-> buf
        (buf-yank a b linewise? true)
        (buf-delete a b)
        set-insert-mode)))

(defn update-x [buf]
  (let [pos (buf :pos)
        r (buf :str)]
    (assoc buf :x (dec (visual-size 
                         (subr r (pos-line-first r pos) (inc pos)) 
                         (buf :tabsize))))))

(defn update-x-if-not-jk
  "update :x unless it is up down motion"
  [buf keycode]
  (let [lastbuf (buf :context :lastbuf)]
    (if-not (or (= (:pos lastbuf) (:pos buf))
                (contains? #{"j" "k" "<c-d>" "<c-u>"} keycode))
      (update-x buf) buf)))

;one server only serve one window at one time

(defn delete-range [buf inclusive? linewise?]
  (let [[a b] (range-prefix buf inclusive?)]
    ;(println "delete-range:" a b)
    (-> buf
        (buf-yank a b linewise? true)
        (buf-delete a b)
        (buf-set-pos a))))

(defn yank-range [buf inclusive? linewise?]
  (let [[a b] (range-prefix buf inclusive?)]
    (buf-yank buf a b linewise?)))

(defn indent-range [buf inclusive?]
  (let [[a b] (range-prefix buf inclusive?)]
    (-> buf
        (buf-indent-lines [a (dec b)]))))

(defn- put-blockwise [buf s append?]
  (let [{r :str pos :pos tabsize :tabsize} buf
        lines (string/split-lines s)
        h (count lines)
        newpos (if append? (-> lines first count (+ pos)) pos)
        buf (reduce (fn [buf [pos r]]
                      (if (neg? pos) buf
                          (buf-insert buf pos r)))
                    buf
                    (reverse (map
                               vector
                               (vertical-line-pos r (if append? (inc pos) pos) h tabsize true)
                               lines)))]
    (buf-set-pos buf newpos)))

(defn- put-linewise [buf s append?]
  (let [{r :str pos :pos} buf
        newpos (if append?
                 (pos-line-last r pos)
                 (pos-line-first r pos))]
    (-> buf
        (buf-insert newpos s)
        (buf-set-pos newpos)
        line-start)))

(defn put-from-register [buf reg append?]
  (let [{s :str res :result linewise? :linewise? blockwise? :blockwise?} (registers-get reg)]
    (cond
      linewise?
      (put-linewise buf s append?)
      blockwise?
      (put-blockwise buf s append?)
      :else
      (let [pos (if append? (inc (buf :pos)) (buf :pos))
            s (if (= reg "=") res s) 
            newpos (-> pos (+ (count s)) dec)]
        (-> buf
            (buf-insert pos s)
            (buf-set-pos newpos))))))

(defn- fire-before-handle-key [buf keycode]
  (fire-event :before-handle-key buf keycode)) 

(defn- fire-after-handle-key [buf keycode]
  (fire-event :after-handle-key buf keycode)) 

(defn apply-keycode [buf keycode]
  (let [keymap (compile-keymap (buf :keymap))
        allkeycode (conj (buf :keys) keycode)
        func (or (keymap (clojure.string/join allkeycode))
                 (keymap (clojure.string/join (conj (buf :keys) ":else")))
                 (if (-> buf :keys empty? not)
                   (or
                     (keymap (clojure.string/join (conj (pop (buf :keys)) ":else" keycode))) ;last :else can be map too
                     (keymap (clojure.string/join (conj (pop (buf :keys)) ":else:else")))))
                 nop)]
    (-> buf
        (fire-before-handle-key keycode)
        (func keycode)
        (fire-after-handle-key keycode))))

(defn apply-keycodes [buf keycodes]
  (reduce apply-keycode buf keycodes))

(defn buf-info [buf]
  (if (and (empty? (buf :str))
           (not (fs/exists? (buf :filepath))))
    (assoc buf :message (str "[New File] " (-> buf :filepath shorten-path)))
    (assoc buf :message (str "\"" (-> buf :filepath shorten-path) "\""))))

(defn- expand-home [f]
  (str (fs/expand-home f)))

(defn path= [f1 f2]
  (try
    (= (str (fs/normalized f1))
       (str (fs/normalized f2)))
    (catch Exception ex
      (println ex)
      false)))

(defn get-panel [create? name]
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

(defn- edit-dir [path]
  (let [abuf (directory-panel)
        files (clojure.string/join "\n"
                                   (map (fn [f] (-> f str shorten-path))
                                        (cons (fs/parent path) (fs/list-dir path))))]
    (send abuf
          (fn [buf]
            (-> buf
                (buf-replace 0 (-> buf :str count) (str files "\n"))
                buf-start
                save-undo
                send-buf!)))
    @abuf))

(defn move-to-line [buf row]
  (-> buf
      (lines-row row)
      line-start))

(defn edit-file
  ([buf file new-file?]
    (if (or (empty? file) (path= file (:filepath buf)))
      buf
      (let [buf-exists (some #(if (path= file (% :filepath)) %)
                             (->> @buffer-list vals (map deref)))
            file (expand-home file)
            newbuf (if (nil? buf-exists)
                     (if (or new-file? (fs/exists? file))
                       (if (fs/directory? file)
                         (edit-dir file)
                         (-> file str new-file deref))
                       nil)
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

(defn goto-buf [buf anextbuf]
  (if (nil? anextbuf) buf
      (let [nextid (@anextbuf :id)
            id (buf :id)]
        (if (= nextid id)  buf
            (do (change-active-buffer id nextid)
                (jump-push buf)
                (assoc buf :nextid nextid))))))

(defn buf-append [buf & strs]
  (buf-insert 
    buf
    (-> buf :str count)
    (apply str strs)))

(defn cursor-center-viewport [buf]
  (assoc buf :scroll-top
         (-> buf :y
             (- (int (/ (-> @ui-agent :viewport :h) 2))))))

(defn append-panel [buf apanel s goto?]
  (send apanel
        (fn [buf]
          (-> buf
              (buf-append s "\n")
              buf-end
              line-start
              save-undo
              cursor-center-viewport
              send-buf!)))
  (if goto? (goto-buf buf apanel) buf))

(defn append-output-panel [buf s goto?]
  (append-panel buf (output-panel) s goto?))

(defn get-buffer-from-reg [reg]
  (if (nil? reg) nil
      (let [abuf (@buffer-list (reg :id))]
        (if (nil? abuf) nil
            abuf))))

(defn start-insert-mode [fnmotion fnedit]
  (fn [buf keycode]
    (-> buf 
        fnmotion
        set-insert-mode
        fnedit)))

(defn normal-mode-fix-pos
  "prevent cursor on top of EOL in normal mode"
  [buf]
  (let [ch (char-at (buf :str) (buf :pos))]
    (if (= (or ch \newline) \newline)
      (char- buf) buf)))

(defn replay-keys [buf keycodes]
  (let [keys (buf :keys)] 
    (-> buf
        (dissoc :keys)
        (apply-keycodes keycodes)
        (assoc :keys keys))))

(defn set-visual-ranges [{{tp :type rg :range} :visual :as buf}]
  ;(println "set-visual-ranges:" tp rg)
  (assoc-in buf [:visual :ranges]
            (condp = tp
              visual-range (list (sort2 rg))
              visual-line (list (make-linewise-range rg buf))
              visual-block (into '() (expand-block-ranges (buf :str) rg (buf :tabsize)))
              nil)))

(defn set-visual-mode [buf visual]
  (-> buf
      (assoc :visual visual)
      set-visual-ranges))

(defn indent-more [buf [a b]]
  (reduce
    (fn [buf [a b]]
      (buf-insert buf a "\t"))
    buf
    (filter 
      (fn [rg]
        (-> buf :str (subr rg) rblank? not))
      (reverse (pos-lines-seq+ (buf :str) a (dec b))))))

(defn- count-leading-space [line]
  (let [[[a b]] (pos-re-seq+ line 0 #"^ *")]
    (- b a)))

(defn indent-less [buf [a b]]
  (reduce
    (fn [{r :str pos :pos :as buf} [a b]]
      (let [line (subr r a b)]
          ;(println "spaces:" (count-leading-space line))
        (cond
          (= (char-at line 0) \tab)
          (buf-delete buf a (inc a))
          :else
          (buf-delete buf a (+ a (min (buf :tabsize) (count-leading-space line)))))))
    buf
    (reverse (pos-lines-seq+ (buf :str) a (dec b)))))

(defn change-case [f]
  (fn [buf [a b]]
    buf [a b] 
    (-> buf
        (buf-replace a b
                     (-> buf
                         :str
                         (subr a b)
                         str
                         f))
        (buf-set-pos a))))

(defn swap-case [^String s]
  (clojure.string/join
    (map (fn [ch]
           (cond
             (Character/isUpperCase ch) (Character/toLowerCase ch)
             (Character/isLowerCase ch) (Character/toUpperCase ch)
             :else ch)) s)))

(defn append-repeat-prefix [buf digit-str]
  (update-in buf [:context :repeat-prefix] #(str % digit-str)))

(defn special-key? [key]
  (contains? #{:enter :leave :before :after :else} key))

(defn wrap-key [keymap key f]
  (update keymap key (fn [handler]
                       (f (or handler nop)))))

(defn wrap-keycode [f]
  (fn [buf keycode]
    (f buf)))

(defn repeat-prefix-value [buf]
  (-> buf :context :repeat-prefix (or "1") parse-int))

(defn current-word [buf]
  (let [{pos :pos
         r :str
         lang :language} buf
        {word-chars :word-chars
         not-word-chars :not-word-chars} (word-re lang)
        re-start (re-pattern (str "([" not-word-chars "](?=[" word-chars "]))|((?<=[" not-word-chars "])$)"))
        re-end (re-pattern (str "[" word-chars "](?=[" not-word-chars "])"))
        b (or (last (pos-re+ r pos re-end)) (count r))
        a (or (last (pos-re- r (dec b) re-start)) 0)]
    (subr r a b)))

(defmacro async [buf & body]
  `(let [abuf# (@buffer-list (~buf :id))]
     (-> abuf#
         (send (fn [~'buf]
                 (let [buf# ~@body]
                   (send-buf! buf#)))))
     ~buf))

(defmacro with-catch [buf & body]
  `(try
     (do ~@body)
     (catch Exception e#
       (assoc ~buf :message (str e#)))))

(defmacro async-with-catch [buf & body]
  `(async ~buf
          (with-catch ~buf ~@body)))

(defn buf-match-bracket
  ([buf pos]
    (-> buf
        (assoc :brackets [])
        (async
          (let [mpos (pos-match-bracket (buf :str) pos)]
            (assoc buf :brackets 
                   (if (nil? mpos) [] [pos mpos]))))))
  ([buf]
    (buf-match-bracket buf (buf :pos))))
