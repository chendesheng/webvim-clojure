(ns webvim.keymap.pair
  (:use webvim.keymap.action
        webvim.keymap.macro
        webvim.keymap.motion
        webvim.keymap.insert
        webvim.keymap.ex
        webvim.core.buffer
        webvim.core.rope
        webvim.core.line
        webvim.core.pos
        webvim.core.register
        webvim.core.serve
        webvim.core.event
        webvim.indent
        webvim.utils
        webvim.jumplist
        webvim.autocompl))

(defn- pair-range[left right around?]
  (fn[buf]
    (println left right around?)
    (let [{r :str pos :pos} buf
          re (re-pattern (quote-pattern right))
          b (first (pos-re+ r pos re))]
      (println b)
      (if (nil? b)
        buf
        (let [a (pos-match-brace r b)]
          (if (nil? a) buf
              (let [rg (if around? [a b] [(inc a) (dec b)])]
                (assoc-in buf [:context :range] rg))))))))

(defn- pair-quotes-range[ch around?]
  (fn[buf]
    (let [{r :str pos :pos} buf
          re (re-pattern (quote-pattern ch))
          b (first (pos-re+ r pos re))]
      (println b)
      (if (nil? b) buf
        (let [a (first (pos-re- r (dec pos) re))]
          (if (nil? a) buf
            (let [rg (if around? [a b] [(inc a) (dec b)])]
              (assoc-in buf [:context :range] rg))))))))

(defn- pair-keymap[around?]
  (merge (reduce-kv
           (fn[keymap left right]
             (-> keymap
                 (assoc left (pair-range left right around?))
                 (assoc right (pair-range left right around?))))
           {}
           {"(" ")"
            "[" "]"
            "{" "}"})
         (reduce
           (fn[keymap ch]
             (-> keymap
                 (assoc ch (pair-quotes-range ch around?))))
           {}
           ["\"" "'" "`"])))
     ;"<" ">"}))

(defn init-pair-keymap[]
  {"a" (pair-keymap true)
   "i" (pair-keymap false)})
;     {"\"" "\""
;     "'" "'"
;     "`" "`"}))
