(ns webvim.indent
  (:use clojure.pprint
        (clojure [string :only (join split blank?)])
        webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.line
        webvim.core.lang
        webvim.core.utils))

(defn auto-indent 
  [r pos]
  (let [lines (filter #(-> % rblank? not)
                      (ranges-to-texts r (pos-lines-seq- r pos)))
        line (first lines)
        pline (second lines)]
    (if (nil? pline) ""
      (or (re-subs #"^\s*" pline) ""))))

(defn- buf-indent-line[buf pos]
  (let [r (buf :str)
        ;_ (println (buf :language))
        indent (indent-pos (buf :language) r pos) 
        a (pos-line-first r pos)
        b (pos-line-start r pos)]
    ;(println a b (count indent))
    (buf-replace buf a b indent)))

(defn buf-indent-lines 
  "indent from cursor row to row both inclusive. Only indent not blank line."
  [buf [a b]]
  (let [r (buf :str)
        lines (filter #(not (rblank? r %)) 
                      (pos-lines-seq+ r a b))
        buf (buf-set-pos buf (-> lines first first))
        lang (buf :language)]
    (line-start
      (first (reduce
               (fn[[buf delta] [pos _]]
                 (let [r (buf :str)
                       a (+ pos delta)
                       b (pos-line-start r a)
                       indent (indent-pos lang r a)]
                   [(buf-replace buf a b indent)
                    ;shift after buffer changed pos
                    (+ delta (- (count indent) (- b a)))])) 
               [buf 0]
               lines)))))

(defn- line-str[r pos]
  (subr r (pos-line r pos)))

(defn buf-indent-current-line
  [buf]
  (println "buf-indent-current-line")
  (let [{r :str pos :pos} buf
        before (line-str r pos)
        buf (buf-indent-line buf pos)
        after (line-str (buf :str) pos)]
    ;press 'o' 'O' then '<esc>' cancel auto indent of cursor line.
    (if (and (= (count before) 1) (> (count after) 1) (rblank? after))
      (update-in buf [:last-indents] conj (pos-line (buf :str) pos))
      buf)))

(defmethod indent-pos :default
  [lang r pos]
  (auto-indent r pos))

(defmethod indent-trigger? :default
  [lang keycode]
  false)
