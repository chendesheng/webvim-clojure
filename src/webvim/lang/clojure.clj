(ns webvim.lang.clojure
  (:use webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.line
        webvim.core.lang
        webvim.indent
        webvim.utils))

(println "load clojure language")

(defonce ^:private listen-new-buffer
  (listen
    :load-language
    (fn [buf]
      (if (= (buf :ext) ".clj")
        (-> buf
            (assoc-in [:language :id] ::clojure)
            (assoc-in [:language :name] "Clojure"))
        buf))))

(defmethod word-re ::clojure [lang]
  (let [word-chars "a-zA-Z_\\-!.?+*=<>&#\\':0-9"
        space-chars "\\s,"]
    {:word-chars word-chars
     :not-word-chars (str "^" word-chars)
     :space-chars space-chars
     :not-space-chars (str "^" space-chars)
     :punctuation-chars (str "^" word-chars space-chars)
     :not-punctuation-chars (str word-chars space-chars) }))

(defn repeat-space[n]
  (reduce (fn[s _]
            (str s " ")) 
          ""
          (range 0 n)))

(def indent-tab-size #{"def" "defn-" "defonce" "defn" "if" "if-not" "nil?" "fn" "let" "cond" "loop"})

(defn clojure-comment? [line]
  (re-test #"^\s*;" line))

(defn clojure-not-blank-or-comment? [line]
  (not (or (rblank? line) (clojure-comment? line))))

(defn clojure-get-indent[line]
  (cond 
    (= 1 (count (.trim line)))
    1
    (not (= (char-at line 0) \())
    1
    (re-test #"^\(\s*[^,\s]+[\s,]+[^,\s]+" line)
    (let [w (re-subs #"[^\s\[\{\(]+"
                     (subr line 1 (count line)))]
      (if (contains? indent-tab-size (str w))
        2
        (-> w count (+ 2))))
    :else 2))

;find outer scope and align by start bracket
(defn clojure-indent
  "Indent by brace parsing"
  [r pos]
  (let [[a b] (pos-line r pos)]
    (cond 
      (zero? a)
      ""
      (clojure-comment? (subr r a b))
      (auto-indent r pos)
      :else (let [tmp (reduce 
                        (fn[stack [a _]]
                          (let [ch (char-at r a)]
                            (if (and (contains? left-braces ch) (empty? stack))
                              (reduced a)
                              (if (= (peek stack) (all-braces ch))
                                (pop stack)
                                (conj stack ch))))) nil 
                        (pos-re-seq- r (dec a) #"(?<!\\)(\(|\[|\{|\}|\]|\))"))
                  mpos (if (number? tmp) tmp nil)]
              (if (nil? mpos)
                ""
                (let [ch (char-at r mpos)
                      [a b] (pos-line r mpos)
                      cnt (- mpos a)]
                  (repeat-space 
                    (+ (- mpos a) 
                       (clojure-get-indent (subr r mpos b))))))))))

(defmethod indent-pos ::clojure
  [lang r pos]
  (println "hello")
  (clojure-indent r pos))
