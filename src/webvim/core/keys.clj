(ns webvim.core.keys
  (:use webvim.core.rope
        webvim.core.pos))

(defn input-keys
  "parse input string to keys (lazy)."
  [s]
  (map (fn [rg] 
         (clojure.string/replace (apply subs s rg) #"\\(.)" "$1")) 
       (pos-re-seq+ (rope s) 0 #"(?<!\\)<.*?(?<!\\)>|\\.|.")))
