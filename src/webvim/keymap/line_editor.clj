(ns webvim.keymap.line-editor
  (:use webvim.core.rope
        webvim.core.pos
        webvim.core.line
        webvim.keymap.action
        webvim.core.utils)) 

(defn- linebuf-update-pos[linebuf a b to]
  (let [pos (linebuf :pos)
        d (- (count to) (- b a))]
    (cond (< pos a) linebuf
          (and (= pos a) (neg? d)) linebuf
          :else (assoc linebuf :pos (+ pos d)))))

(defn- linebuf-replace[linebuf a b to]
  ;(println a b to)
  (if (> a b) linebuf
      (-> linebuf
          (update-in [:str] replacer a b to)
          (linebuf-update-pos a b to))))

(defn- update-linebuf[buf f]
  (let [linebuf (buf :line-buffer)]
    (update-in buf [:line-buffer] f)))

(defn- linebuf-insert[buf r]
  (update-linebuf 
    buf 
    (fn[linebuf]
      (let [pos (linebuf :pos)]
        (linebuf-replace linebuf pos pos r)))))

(defn- linebuf-delete[buf offset]
  (update-linebuf 
    buf 
    (fn[linebuf]
      (let [pos (linebuf :pos)
            r (linebuf :str)
            [a b] (sort2 pos (+ pos offset))
            a1 (bound-range a 0 (count r))
            b1 (bound-range b 0 (count r))]
        (linebuf-replace linebuf a1 b1 "")))))

(defn- linebuf-move 
  [buf fnmove]
  (update-linebuf 
    buf 
    (fn[linebuf]
      (let [pos (linebuf :pos)
            r (linebuf :str)
            newpos (or (fnmove r pos) pos)]
        (assoc linebuf :pos (bound-range newpos 0 (count r)))))))

(defn- linebuf-char+[buf]
  (linebuf-move buf (fn [r pos] (inc pos))))

(defn- linebuf-char-[buf]
  (linebuf-move buf (fn [r pos] (dec pos))))

(defn- linebuf-start[buf]
  (linebuf-move buf (fn [r pos] 0)))

(defn- linebuf-end[buf]
  (linebuf-move buf (fn [r pos] (count r))))

(defn- line-editor-default[buf keycode]
  (let [ch (keycode-to-char keycode)]
    (linebuf-insert buf ch)))

(defn- line-editor-continue[buf keycode]
  (not (or
         (-> buf :line-buffer nil?)
         (contains? #{"<cr>" "<esc>"} keycode))))

(defn- line-editor-leave [buf keycode] 
  (-> buf
      (dissoc :line-buffer)
      (assoc :message (or (buf :message) "")))) ;Make sure got :message filled

(defn- line-editor-<bs>
  [{{r :str} :line-buffer :as buf}] 
  (if (empty? r)
    (dissoc buf :line-buffer)
    (linebuf-delete buf -1)))

(defn- line-editor-put[buf keycode]
  (let [txt (-> buf (get-register keycode) :str)]
    (if (string? txt)
      (linebuf-insert buf (-> txt rope first-line .trimEnd str))
      buf)))

(defn- line-editor-<c-w>
  [{{r :str pos :pos} :line-buffer :as buf}]
  (let [newpos (or (first (pos-re- r pos #"(?<=\s|^)\S")) pos)]
    (linebuf-delete buf (- newpos pos))))

(defn init-line-editor-keymap[]
  {"<c-f>" linebuf-char+
   "<c-b>" linebuf-char-
   "<c-a>" linebuf-start
   "<c-e>" linebuf-end
   "<bs>" line-editor-<bs>
   "<c-h>" line-editor-<bs>
   "<c-d>" #(linebuf-delete % 1)
   "<c-r>" {"<esc>" identity
            :else line-editor-put}
   "<c-w>" line-editor-<c-w>
   :enter line-editor-enter
   :else line-editor-default
   :continue line-editor-continue
   :leave line-editor-leave})
