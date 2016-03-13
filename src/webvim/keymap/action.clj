;common actions
(ns webvim.keymap.action
  (:require [me.raynes.fs :as fs]
            [clojure.string :as string]
            [webvim.mode :refer [set-insert-mode set-normal-mode]]
            [webvim.visual :refer [visual-range visual-line visual-block]]
            [webvim.scrolling :refer [cursor-center-viewport]])
  (:use webvim.core.buffer
        webvim.core.rope
        webvim.core.line
        webvim.core.pos
        webvim.core.register
        webvim.core.event
        webvim.fuzzy
        webvim.core.lang
        webvim.core.utils
        webvim.jumplist
        webvim.keymap.compile
        webvim.core.ui
        clojure.pprint))

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

(defn keycode-cancel [buf]
  (-> buf
      set-normal-mode
      (dissoc :context :keys :line-buffer)
      (assoc :visual {:type 0 :range [0 0]}
             :message ""
             :autocompl nil
             :showkeys nil)))

(defn- fire-before-handle-key [buf keycode]
  (fire-event :before-handle-key buf keycode)) 

(defn- fire-after-handle-key [buf keycode]
  (fire-event :after-handle-key buf keycode)) 

(defn apply-keycode [buf keycode]
  (if (= keycode "<c-c>")
    (keycode-cancel buf)
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
          (fire-after-handle-key keycode)))))

(defn apply-keycodes [buf keycodes]
  (reduce apply-keycode buf keycodes))

(defn- expand-home [f]
  (str (fs/expand-home f)))

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
      (let [buf-exists (some #(if (and (-> % :filepath nil? not)
                                       (path= file (% :filepath))) %)
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
  (if goto? (goto-buf buf apanel) buf))

(defn append-output-panel [buf s goto?]
  (append-panel buf (output-panel) s goto?))

(defn get-buffer-from-reg [reg]
  (if (nil? reg) nil
      (let [abuf (@buffer-list (reg :id))]
        (if (nil? abuf) nil
            abuf))))

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
