(ns webvim.core.utils
  (:require [me.raynes.fs :as fs]
            [clojure.string :as string]))

(defn quote-pattern [ch]
  (java.util.regex.Pattern/quote (str ch)))

(defn sort2
  ([a b]
    (if (< a b) [a b] [b a]))
  ([[a b]]
    (sort2 a b)))
;Variation Selectors 
(defn variation-selector? [ch]
  (and
    (some? ch)
    (<= 0xFE00 (int ch) 0xFEFF)))

;https://en.wikipedia.org/wiki/UTF-16#U.2BD800_to_U.2BDFFF
(defn surrogate? [ch]
  (and
    (some? ch)
    (<= 0xD800 (int ch) 0xDFFF)))

(defn keycode-to-char [keycode]
  (cond
    (= 1 (count keycode))
    keycode
    (and (= 2 (count keycode)) (-> keycode first surrogate?))
    keycode
    (= "<cr>" keycode)
    "\n"
    (= "<tab>" keycode)
    "\t"
    (= "<space>" keycode)
    " "
    :else nil))

(defn make-range
  ([a b inclusive?]
    (if inclusive?
      (let [[a b] (sort2 a b)]
        [a (inc b)])
      (sort2 a b)))
  ([[a b] inclusive?]
    (make-range a b inclusive?)))

(defn bound-range [v r e]
  (cond (<= v r) r
        (>= v e) e
        :else v))

;http://dev.clojure.org/jira/browse/CLJ-1468
(defn deep-merge
  "Like merge, but merges maps recursively."
  [& maps]
  (if (every? map? maps)
    (apply merge-with deep-merge maps)
    (last maps)))

;"tree" actually means nested map like {:a {:aa "haha" :c {:cc "ccc"} } :b "hehe"}
;(map? node) equals true then node is a branch node (or a sub tree)
;(map? node) equals false then node is a leaf
(defn- tree-reduce-recur [visit-branch visit-leaf path ctx root]
  (reduce
    (fn [ctx [k v :as node]]
      (let [path (conj path node)]
        (if (map? v)
          (visit-branch
            (tree-reduce-recur
              visit-branch visit-leaf path ctx v) path)
          (visit-leaf ctx path)))) ctx root))

;deep first order
(defn tree-reduce [visit-branch visit-leaf ctx root]
  (tree-reduce-recur visit-branch visit-leaf (into '() {"" root}) ctx root))

(defn tree-map
  "only map leaf nodes"
  [f root]
  (tree-reduce-recur
    (fn [ctx path]
      ctx)
    (fn [ctx path]
      ;(println path)
      (let [ks (reverse (map key path))]
        ;(println (get-in root ks))
        (assoc-in ctx ks (f (reverse ks) (get-in ctx ks)))))
    nil root root))

;(tree-map (fn[item]
;            (println item)
;            (clojure.string/reverse item)) {:a {:empty {} :aa "haha" :c {:cc "123"} } :b "hehe"})

;(tree-reduce 
;  (fn[ctx path]
;    ;(println (first path))
;    ctx)
;  (fn[ctx path]
;    (str ctx (-> path first val))) "" {:a {:aa "haha" :c {:cc "ccc"} } :b "hehe"})

(defn parse-int [s]
  (Integer. (re-find #"\d+" (or s "0"))))

(defn vconj
  [coll & xs]
  (into (or coll []) xs))

(defn crlf? [txt]
  (-> #"\r?\n"
      (re-seq txt)
      first
      count
      (= 2)))

(defn- round-to-tabstop [n tabsize]
  (+ n (- tabsize (rem n tabsize))))

(defn visual-size [s tabsize]
  (let [len (count s)]
    (if (empty? s) 0
        (loop [i 0 ret 0]
          (if (>= i len)
            ret
            (let [nexti (.indexOf s "\t" i)]
              (if (neg? nexti)
                (+ ret (- len i))
                (recur (inc nexti)
                       (+ ret (round-to-tabstop (- nexti i) tabsize))))))))))

(comment
  (defn test-visual-size []
    [(visual-size " \t  " 4)
     (visual-size "    \t" 4)
     (visual-size "  \t  " 4)
     (visual-size "\t\t  " 4)
     (visual-size "    \t\t" 4)
     (visual-size "   \t" 4)
     (visual-size "     \t \t" 4)]))

(defn visualx-to-charx [^String s vx tabsize]
  (let [chars (char-array s)]
    (loop [i 0 vxi 0]
      (let [vxi (if (= (aget chars i) \tab)
                  (+ vxi (- tabsize (rem vxi tabsize)))
                  (inc vxi))]
        (cond
          (> vxi vx) i
          (= vxi vx) (inc i)
          (>= i (-> s count dec)) (inc i)
          :else (recur (inc i) vxi))))))

(defn nop [buf keycode] buf)

(defn quote-patterns [& args]
  (clojure.string/join "|" (map quote-pattern args)))

(defn repeat-chars [n ch]
  (clojure.string/join (repeat n ch)))

(def windows?
  (-> (System/getProperty "os.name")
      (.indexOf "Windows")
      (>= 0)))

(def osx?
  (-> (System/getProperty "os.name")
      (.indexOf "Mac OS X")
      (>= 0)))

(defn clipboard-get []
  (if osx?
    ((clojure.java.shell/sh "pbpaste") :out)
    ""))

;TODO: use xclip in linux
(defn clipboard-set! [text]
  (cond
    osx?
    (clojure.java.shell/sh "pbcopy" :in text)
    windows?
    (clojure.java.shell/sh "clip" :in text)))

(defn- replace-path-sep [f]
  (if windows?
    (string/replace (str f) "/" "\\")
    f))

(defn expand-path [f]
  (-> f
      replace-path-sep
      fs/expand-home
      fs/normalized))

(defn path= [f1 f2]
  (try
    (= (expand-path f1)
       (expand-path f2))
    (catch Exception ex
      (println ex)
      false)))

(comment
  (webvim.core.utils/visual-size "\t\ta" 5)
  (webvim.core.utils/visualx-to-charx "\t\t345" 3 4))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn shorten-path
  [^String path]
  (if (some? path)
    (if (fs/child-of? fs/*cwd* path)
      (subs path (-> fs/*cwd* str count inc))
      (let [home (fs/home)]
        (if (fs/child-of? home path)
          (str "~" (subs path (-> home str count)))
          path)))))

(defmacro with-ns
  "Evaluates body in another namespace.  ns is either a namespace
  object or a symbol.  This makes it possible to define functions in
  namespaces other than the current one."
  [ns & body]
  `(binding [*ns* (the-ns ~ns)]
     ~@(map (fn [form] `(eval '~form)) body)))

;https://clojure.github.io/clojure-contrib/with-ns-api.html
(defmacro with-temp-ns
  "Evaluates body in an anonymous namespace, which is then immediately
  removed.  The temporary namespace will 'refer' clojure.core."
  [& body]
  `(try
     (create-ns 'sym#)
     (let [result# (with-ns 'sym#
                            (clojure.core/refer-clojure)
                            ~@body)]
       result#)
     (finally (remove-ns 'sym#))))

;FIXME: This is too hacky
(defn get-namespace [filepath]
  "get namespace under webvim by file path"
  (let [filepath (or filepath "")
        [[_ _ _ nm]] (re-seq #"(?i)webvim[/\\](src|dev|src-cljc|src-cljs)(/|\\)(.+)\.cljc?" filepath)]
    (if (empty? nm)
      nil
      (-> nm
          (string/replace "/" ".")
          (string/replace "\\" ".")
          (string/replace "_" "-")))))

(defn pretty-trace
  "convert error trace to file path with line number"
  [err]
  (let [st (-> err .getStackTrace seq)]
    (map (fn [line]
           (let [class (.getClassName line)
                ;method (.getMethodName line)
                 file (.getFileName line)
                 linenum (.getLineNumber line)
                 appendsrc (fn [name]
                             (if (nil? (re-seq #"^webvim" name))
                               name
                               (str "src/" name)))]
             (str (-> class
                      (string/replace #"\$.*" "")
                      ;FIXME: handle class name doesn't contains dot 
                      (string/replace #"[.][^.]*$" "")
                      (string/replace "." "/")
                      (string/replace "_" "-")
                      appendsrc) "/" file ":" linenum))) st)))

(defmacro nilor
  "Like `or` but check nil?"
  ([] nil)
  ([x] x)
  ([x & next]
    `(let [or# ~x]
       (if (some? or#) or# (or ~@next)))))

(defmacro print-stack []
  (.printStackTrace (Exception.)))

(defn trim-last-newline [s]
  (let [idx (dec (count s))]
    (if (= (.charAt s idx) \newline)
      (subs s 0 idx)
      s)))

(defn split-lines [s]
  (string/split s #"(?<=\n)"))

(defn ensure-ends-with-newline [s]
  (if (-> s last (= \newline))
    s
    (str s \newline)))

(defn keymap-comp [f g]
  (fn [buf keycode]
    (-> buf
        (g keycode)
        (f keycode))))

(defn keymap-do-before [fprint f]
  (fn [buf keycode]
    (fprint buf keycode)
    (f buf keycode)))

(defn with-temp-file [s f]
  (let [file (str (fs/temp-file "webvim"))]
    (try
      (spit file s)
      (f file)
      (finally
        (fs/delete file)))))

(comment defmacro with-temp-file [s f & body]
         `(let [~f (str (fs/temp-file "webvim"))]
            (try
              (spit ~f s)
              ~@body
              (finally
                (fs/delete ~f)))))
