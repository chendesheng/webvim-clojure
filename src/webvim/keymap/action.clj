;common actions
(ns webvim.keymap.action
  (:require [me.raynes.fs :as fs])
  (:use webvim.core.buffer
        webvim.core.rope
        webvim.core.line
        webvim.core.pos
        webvim.core.register
        webvim.indent
        webvim.core.utils
        webvim.jumplist
        webvim.core.ui))

(defonce root-keymap (atom {}))

(def normal-mode 0)
(def insert-mode 1)
(def visual-mode 2)
(def ex-mode 3)

(def visual-normal 0)
(def visual-line 1)
(def visual-block 2) ;TODO

(defn get-register[buf c]
  (registers-get (buf :registers) c))

(defn put-register![buf c v]
  (registers-put! (buf :registers) c v))

(defn file-register[buf]
  {:id (buf :id) :str (or (buf :filepath) (buf :name))})

(defn pos-match-brace
  "return matched brace position, nil if not find"
  [r pos]
  (let [brace (char-at r pos)
        m (all-braces brace)
        left? (contains? left-braces brace)
        re (re-pattern (str  (quote-pattern brace) "|" (quote-pattern m)))]
    (if (nil? m) nil
      (let [inc-cnt? (if left?
                       #(contains? left-braces %)
                       #(contains? right-braces %))
            braces (if left?
                     (pos-re-seq+ r pos re)
                     (pos-re-seq- r pos re))
            mpos (reduce
                   (fn[cnt [a _]]
                     (let [ch (char-at r a)
                           newcnt (if (inc-cnt? ch)
                                    (inc cnt)
                                    (dec cnt))]
                       (if (zero? newcnt)
                         (reduced [a])
                         newcnt))) 0 braces)]
        (if (vector? mpos) (first mpos) nil)))))

(defn buf-update-highlight-brace-pair[buf pos]
  (let [mpos (pos-match-brace (buf :str) pos)]
    ;(println pos mpos)
    (if (nil? mpos)
      (assoc buf :braces [])
      (assoc buf :braces [pos mpos]))))

(defn- add-highlight[buf rg]
  (let [highlights (buf :highlights)]
    (if (empty? (filter (fn[[a b]]
                          (and (= a (rg 0)) (= b (rg 1)))) highlights))
      (update-in buf [:highlights] conj rg) buf)))

(defn re-forward-highlight[buf re]
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

(defn re-backward-highlight[buf re]
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

(defn set-insert-mode[buf keycode]
  ;(println "set-insert-mode")
  (merge buf {:mode insert-mode
              :message nil
              :visual {:type 0 :ranges nil}}))

(defn set-normal-mode[buf]
  ;(println "set-normal-mode:")
  (merge buf {:mode normal-mode
              :visual {:type 0 :ranges nil}
              :autocompl {:suggestions nil
                          :suggestions-index 0}}))

(defn buf-yank[buf a b linewise?]
  (let [s (buf-subr buf a b)]
    (put-register! buf (-> buf :context :register) {:str s :linewise? linewise?})
    (update-in buf [:context] dissoc :register)))

(defn change-active-buffer[id nextid]
  (let [path-name #(or (% :filepath) (% :name))]
    (if (not= id nextid)
      (do
        (registers-put! registers "#" 
                        (file-register
                          (-> @buffer-list (get id) deref)))
        (registers-put! registers "%"
                        (file-register
                          (-> @buffer-list (get nextid) deref)))))))


;collect range argument, TODO: add linewise
(defn range-prefix[buf inclusive?]
  (cond
    (-> buf :mode (= visual-mode))
    (let [tp (-> buf :visual :type)]
      (cond
        (= tp visual-normal)
        (-> buf :visual :ranges (get 0) (make-range inclusive?))
        (= tp visual-line)
        (-> buf :visual :ranges (get 0) (make-linewise-range buf))
        (= tp visual-block)
        (throw (Exception. "TODO: visual-block"))))
    (-> buf :context :range nil? not)
    (-> buf :context :range (make-range inclusive?))
    :else (throw (Exception. "no range prefix exist"))))

(defn change-range[buf inclusive? linewise?]
  (let [[a b] (range-prefix buf inclusive?)]
    (-> buf
        (buf-yank a b linewise?)
        (buf-delete a b)
        (set-insert-mode "c"))))

(defn update-x[buf]
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
                (contains? #{"j" "k" "<c+d>" "<c+u>"} keycode))
      (update-x buf) buf)))

;one server only serve one window at one time

(defn cursor-center-viewport[buf]
  (assoc buf :scroll-top
            (-> buf :y
                (- (int (/ (-> @ui-agent :viewport :h) 2))))))

(defn delete-range[buf inclusive? linewise?]
  (let [[a b] (range-prefix buf inclusive?)]
    (println "delete-range:" a b)
    (-> buf
        (buf-yank a b linewise?)
        (buf-delete a b)
        (buf-set-pos a))))

(defn yank-range[buf inclusive? linewise?]
  (let [[a b] (range-prefix buf inclusive?)]
    (buf-yank buf a b linewise?)))

(defn indent-range[buf inclusive?]
  (let [[a b] (range-prefix buf inclusive?)]
    (-> buf
        (buf-indent-lines [a b]))))

(defn put-from-register[buf keycode]
  (let [{s :str linewise? :linewise?} (get-register buf keycode)]
    (if linewise?
      (let [{r :str pos :pos} buf
            a (pos-line-first r pos)]
        (-> buf
            (buf-insert a s)
            (buf-set-pos a)
            line-start))
      (-> buf
          (buf-insert s)
          char-))))

(defn put-from-register-append[buf keycode]
  (let [{s :str linewise? :linewise?} (get-register buf keycode)
        pos (buf :pos)]
    (if linewise?
      (let [r (buf :str)
            b (pos-line-last r pos)]
        (-> buf
            (buf-insert b s)
            (buf-set-pos b)
            line-start))
      (-> buf
          (buf-insert (inc pos) s)
          (buf-set-pos (+ pos (count s)))))))

(defn nop[buf keycode] buf)

(defn apply-keycode[buf keycode keymap]
  (let [allkeycode (conj (buf :keys) keycode)
        ;_ (println (buf :keys))
        ;_ (println allkeycode)
        func (or (keymap (clojure.string/join allkeycode))
                 (keymap (clojure.string/join (conj (buf :keys) ":else")))
                 (if (-> buf :keys empty? not)
                   (or
                     (keymap (clojure.string/join (conj (pop (buf :keys)) ":else" keycode))) ;last :else can be map too
                     (keymap (clojure.string/join (conj (pop (buf :keys)) ":else:else")))))
                 nop)]
    (func buf keycode)))

(defn apply-keycodes[buf keymap keycodes]
  (reduce
    (fn[[buf changes] keycode]
      (let [newbuf (apply-keycode buf keycode keymap)
            newchanges (newbuf :changes)]
        [(assoc newbuf :changes [])
         (concat changes newchanges)])) [buf nil] keycodes))

(defn move-to-jumplist
  [buf fndir]
  (loop [pos (fndir buf)]  ;TODO: filter lazy seq instead of loop
    (if (nil? pos)
      buf ;newest or oldest
      (let [newb @(@buffer-list (pos :id))]
        (if (nil? newb)
          ;buffer has been deleted, ignore
          (recur (fndir buf))
          ;pos is avaliable
          (if (< (pos :pos) (count (newb :str)))
            (let [id (buf :id)
                  newid (pos :id)
                  newpos (pos :pos)]
              (if (= newid id)
                ;update pos inside current buffer
                (buf-set-pos buf newpos)
                (let []
                  (change-active-buffer id newid)
                  ;(swap! buffer-list update-in [newid] #(buf-set-pos % newpos))
                  (assoc buf :nextid newid))))
            ;buffer has been modifed and cursor is no longer inside, ignore
            (recur (fndir buf))))))))

(defn buf-info[buf]
  (if (and (empty? (buf :str))
           (not (fs/exists? (buf :filepath))))
    (assoc buf :message (str "[New File] " (-> buf :filepath shorten-path)))
    (assoc buf :message (str "\"" (-> buf :filepath shorten-path) "\""))))

(defn- expand-home[f]
  (str (fs/expand-home f)))

(defn path=[f1 f2]
  (try
    (= (str (fs/normalized f1))
       (str (fs/normalized f2)))
    (catch Exception ex
      (println ex)
      false)))

(defn edit-file[buf file new-file?]
  (if (or (empty? file) (path= file (:filepath buf)))
    buf
    (let [buf-exists (some #(if (path= file (% :filepath)) %)
                           (->> @buffer-list vals (map deref)))
          newbuf (if (nil? buf-exists)
                   (if (or new-file? (-> file expand-home fs/exists?))
                     (-> file expand-home str new-file deref)
                     nil)
                   buf-exists)]
      (if (nil? newbuf) buf
        (let [newid (newbuf :id)]
          (change-active-buffer (buf :id) newid)
          (jump-push buf)
          (assoc buf :nextid newid))))))

(defn move-to-line[buf row]
  (-> buf
      (lines-row row)
      line-start))

(defn output-buf[create?]
  (or (some (fn[[_ abuf]]
              (if (= (@abuf :name) output-buf-name) abuf nil))
            @buffer-list)
      (if create?
        (-> (open-file output-buf-name)
            (assoc :root-keymap @root-keymap)
            buffer-list-save!))))

(defn goto-buf [buf anextbuf]
  (if (nil? anextbuf) buf
    (let [nextid (@anextbuf :id)
          id (buf :id)]
      (if (= nextid id)  buf
        (do (change-active-buffer id nextid)
            (jump-push buf)
            (assoc buf :nextid nextid))))))

(defn buf-append[buf & strs]
  (buf-insert 
    buf
    (-> buf :str count)
    (apply str strs)))

(defn write-output[buf s goto?]
  (let [aoutputbuf (output-buf true)]
    (send aoutputbuf
          (fn[buf]
            (let [row (buf :linescnt)
                  newbuf (-> buf
                             (buf-append s "\n")
                             (move-to-line row)
                             cursor-center-viewport)]
              (send-buf! newbuf))))
    (if goto? (goto-buf buf aoutputbuf) buf)))

(defn get-buffer-from-reg[reg]
  (if (nil? reg) nil
    (let [abuf (@buffer-list (reg :id))]
      (if (nil? abuf) nil
        abuf))))

