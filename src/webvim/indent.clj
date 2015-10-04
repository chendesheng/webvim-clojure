(ns webvim.indent
  (:use clojure.pprint
        (clojure [string :only (join split blank?)])
        webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.line
        webvim.core.lang
        webvim.utils))

(defn auto-indent 
  [r pos]
  (let [lines (filter #(-> % rblank? not)
                      (ranges-to-texts r (pos-lines-seq- r pos)))
        line (first lines)
        pline (second lines)]
    (if (nil? pline) ""
      (or (re-subs #"^\s*" pline) ""))))

(defn buf-indent-line[buf pos]
  (let [r (buf :str)
        _ (println (buf :language))
        indent (indent-pos (buf :language) r pos) 
        a (pos-line-first r pos)
        b (pos-line-start r pos)]
    ;(println a b)
    (buf-replace buf a b indent)))

(defn buf-indent-lines 
  "indent from cursor row to row both inclusive. Only indent not blank line."
  [buf [a b]]
  (let [r (buf :str)
        pos (buf :pos)
        lines (pos-lines-seq+ r a b)]
    (line-start
      (reduce
        #(buf-indent-line %1 (first %2)) 
        (buf-set-pos buf (-> lines first first))
        (filter #(not (rblank? r %)) lines)))))

(defn buf-indent-current-line
  [buf]
  (buf-indent-line buf (buf :pos)))

;(def language-indents 
;  {"Clojure" {:fn-indent clojure-indent}
;   "JavaScript" {:fn-indent clang-indent
;          :indent-triggers #"}"}
;   "CSS" {:fn-indent clang-indent
;           :indent-triggers #"}"}
;   "XML" {:fn-indent auto-indent}
;   :else {:fn-indent auto-indent}})

(defmethod indent-pos :default
  [lang r pos]
  (auto-indent r pos))

(defmethod indent-trigger? :default
  [lang keycode]
  false)
