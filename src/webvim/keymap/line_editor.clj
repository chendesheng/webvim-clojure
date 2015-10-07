(ns webvim.keymap.line-editor
  (:use webvim.core.rope
        webvim.core.pos
        webvim.core.line
        webvim.utils)) 

(defn- linebuf-update-pos[linebuf a b to]
  (let [pos (linebuf :pos)]
    (if (< pos a) linebuf
      (assoc linebuf :pos (+ pos (- (count to) (- b a)))))))

(defn- linebuf-replace[linebuf a b to]
  (println a b to)
  (if (> a b) linebuf
    (let [r (linebuf :str)]
      (-> linebuf
          (assoc :str (replacer r a b to))
          (linebuf-update-pos a b to)))))

(defn- update-linebuf[buf f]
  (let [linebuf (buf :line-buffer)]
    (update-in buf [:line-buffer] f)))

(defn- linebuf-insert[buf r]
  (update-linebuf buf (fn[linebuf]
                        (let [pos (linebuf :pos)]
                          (linebuf-replace linebuf pos pos r)))))

(defn- linebuf-delete[buf offset]
  (update-linebuf buf (fn[linebuf]
                        (let [pos (linebuf :pos)
                              r (linebuf :str)
                              [a b] (sort2 pos (+ pos offset))
                              a1 (bound-range a 1 (count r))
                              b1 (bound-range b 1 (count r))]
                          (linebuf-replace linebuf a1 b1 "")))))

(defn- linebuf-move 
  [buf fnmove]
  (update-linebuf 
    buf 
    (fn[linebuf]
      (let [pos (linebuf :pos)
            r (linebuf :str)
            newpos (or (fnmove r pos) pos)]
        (assoc linebuf :pos (bound-range newpos 1 (count r)))))))

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

(defn init-line-editor-keymap[]
  {"<c+f>" linebuf-char+
   "<c+b>" linebuf-char-
   "<c+a>" linebuf-start
   "<c+e>" linebuf-end
   "<bs>" #(linebuf-delete % -1)
   "<c+h>" #(linebuf-delete % -1)
   "<c+d>" #(linebuf-delete % 1)
   ;"<c+w>" #(linebuf-replace % pos (pos-re- (% :str) #"\s") "")
   :else line-editor-default
   :continue (fn[buf keycode] true)
   :leave (fn[buf keycode] (dissoc buf :line-buffer))})
