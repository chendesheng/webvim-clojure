(ns webvim.keymap.linebuf.linebuf
  (:require 
    [webvim.keymap.objects :refer [current-word current-WORD]]
    [webvim.keymap.compile :refer [wrap-key wrap-keycode]])
  (:use webvim.core.rope
        webvim.core.pos
        webvim.core.line
        webvim.core.register
        webvim.core.parallel-universe
        webvim.keymap.linebuf.history
        webvim.core.utils)) 

(defn- set-line-buffer [buf s]
  (-> buf
      (assoc-in [:line-buffer :str] (rope s))
      (assoc-in [:line-buffer :pos] (count s))))

(defn- recover-command-history [buf ahistory]
  (let [s (@ahistory next-future)]
    (if (nil? s) 
      buf
      (set-line-buffer buf s))))

(defn- save-history! [ahistory s]
  (if-not (or (empty? s) (= (just-now @ahistory) s))
    (swap! ahistory 
           #(-> %
                fast-forward
                (new-future s)))))

(defn- linebuf-update-pos [linebuf a b to]
  (let [pos (linebuf :pos)
        d (- (count to) (- b a))]
    (cond (< pos a) linebuf
          (and (= pos a) (neg? d)) linebuf
          :else (assoc linebuf :pos (+ pos d)))))

(defn- match-bracket
  ([{r :str pos :pos :as linebuf} insert?]
    (let [pos (if insert? (dec pos) pos)
          pos2 (pos-match-bracket r pos)]
      (if (nil? pos2)
        (assoc linebuf :pos2 nil)
        (assoc linebuf :pos2 pos2)))))

(defn- linebuf-replace [linebuf a b to]
  ;(println a b to)
  (if (> a b) linebuf
      (-> linebuf
          (update :str replacer a b to)
          (linebuf-update-pos a b to)
          (match-bracket true))))

(defn- update-linebuf [buf f]
  (let [linebuf (buf :line-buffer)]
    (update buf :line-buffer f)))

(defn- linebuf-insert [buf r]
  (if (empty? r) buf
      (update-linebuf 
        buf 
        (fn [linebuf]
          (let [pos (linebuf :pos)]
            (-> linebuf
                (linebuf-replace pos pos r)))))))

(defn- linebuf-delete [buf offset]
  (update-linebuf 
    buf 
    (fn [linebuf]
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
    (fn [linebuf]
      (let [pos (linebuf :pos)
            r (linebuf :str)
            newpos (bound-range (or (fnmove r pos) pos) 0 (count r))]
        (-> linebuf
            (assoc :pos newpos)
            (match-bracket false))))))

(defn- linebuf-char+ [buf keycode]
  (linebuf-move buf (fn [r pos] (inc pos))))

(defn- linebuf-char- [buf keycode]
  (linebuf-move buf (fn [r pos] (dec pos))))

(defn- linebuf-start [buf keycode]
  (linebuf-move buf (fn [r pos] 0)))

(defn- linebuf-end [buf keycode]
  (linebuf-move buf (fn [r pos] (count r))))

(defn- linebuf-default [buf keycode]
  (let [ch (keycode-to-char keycode)]
    (linebuf-insert buf ch)))

(defn- linebuf-enter [buf keycode]
  ;(println "linebuf-enter:" keycode)
  (let [buf (dissoc buf :message)]
    (if (-> buf :line-buffer nil?)
      (assoc buf :line-buffer {:prefix keycode
                               :str (rope "")
                               :pos 0})
      buf)))

(defn- linebuf-continue [buf keycode]
  (not (or
         (-> buf :line-buffer nil?)
         (contains? #{"<cr>" "<esc>"} keycode))))

(defn- linebuf-leave [buf keycode] 
  (-> buf
      (dissoc :line-buffer)
      (assoc :message (or (buf :message) "")))) ;Make sure :message get value

(defn- linebuf-<bs>
  [{{r :str} :line-buffer :as buf} keycode] 
  (if (empty? r)
    (dissoc buf :line-buffer)
    (linebuf-delete buf -1)))

(defn- linebuf-put [buf keycode]
  (let [txt (:str (registers-get keycode))]
    (if (string? txt)
      (linebuf-insert buf (-> txt rope first-line .trimEnd str))
      buf)))

(defn- linebuf-<c-w>
  [{{r :str pos :pos} :line-buffer :as buf} keycode]
  (let [newpos (or (first (pos-re- r pos #"(?<=\s|^)\S")) pos)]
    (linebuf-delete buf (- newpos pos))))

(defn- linebuf-<c-u>
  [buf keycode]
  (linebuf-delete buf (-> buf :pos -)))

(defn init-linebuf-keymap
  ([ahistory]
    (-> (init-history-keymap ahistory)
        (wrap-key :leave
                  (fn [handler]
                    (fn [buf keycode]
                      (-> buf
                          (handler keycode)
                          (linebuf-leave keycode)))))
        (assoc "<c-f>" linebuf-char+
               "<c-b>" linebuf-char-
               "<c-a>" linebuf-start
               "<c-e>" linebuf-end
               "<bs>" linebuf-<bs>
               "<c-h>" linebuf-<bs>
               "<c-d>" (wrap-keycode #(linebuf-delete % 1))
               "<c-r>" {"<esc>" nop
                        "<c-w>" (fn [buf keycode]
                                  (linebuf-insert buf (current-word buf)))
                        "<c-a>" (fn [buf keycode]
                                  (linebuf-insert buf (current-WORD buf)))
                        :else linebuf-put}
               "<c-w>" linebuf-<c-w>
               "<c-u>" linebuf-<c-u>
               :enter linebuf-enter
               :else linebuf-default
               :continue linebuf-continue)))
  ([]
    (init-linebuf-keymap (atom (parallel-universe)))))

