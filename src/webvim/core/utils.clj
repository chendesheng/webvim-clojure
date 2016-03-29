(ns webvim.core.utils
  (:require [snipsnap.core :as clipboard]
            [me.raynes.fs :as fs]))

(defn quote-pattern [ch]
  (java.util.regex.Pattern/quote (str ch)))

(defn sort2
  ([a b]
    (if (< a b) [a b] [b a]))
  ([[a b]]
    (sort2 a b)))

(defn keycode-to-char [keycode]
  (cond 
    (= 1 (count keycode))
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

(defn vconj [coll x]
  (conj (or coll []) x))

(defn crlf? [txt]
  (let [[m] (re-seq #"\r?\n" txt)]
    (= (count m) 2)))


(defn visual-size [s tabsize]
  (let [len (count s)]
    (if (empty? s) 0
        (loop [i 0 ret 0]
          (if (>= i len)
            ret
            (let [nexti (.indexOf s "\t" i)]
              (if (neg? nexti)
                (+ ret (- (count s) i))
                (recur (inc nexti) 
                       (+ ret (- tabsize (rem ret tabsize)))))))))))

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
    (clipboard/get-text)))

;TODO: use xclip in linux
(defn clipboard-set! [text]
  (cond
    osx?
    (clojure.java.shell/sh "pbcopy" :in text)
    windows?
    (clojure.java.shell/sh "clip" :in text)
    :else (clipboard/set-text! text)))

(defn path= [f1 f2]
  (try
    (= (str (fs/normalized f1))
       (str (fs/normalized f2)))
    (catch Exception ex
      (println ex)
      false)))

(comment
  (webvim.core.utils/visual-size "\t\ta" 5)
  (webvim.core.utils/visualx-to-charx "\t\t345" 3 4))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn shorten-path [^String path]
  (if-not (nil? path)
    (if (fs/child-of? fs/*cwd* path)
      (subs path (-> fs/*cwd* str count inc))
      path)))

