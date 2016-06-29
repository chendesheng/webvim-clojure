(ns webvim.core.lineindex
  "log(n) time get line number from pos or get pos range from line number"
  (:require [clojure.string :as string]
            [webvim.core.utils :refer [negzero split-lines]]
            [clojure.pprint :refer [pprint]]))

(defn- make-tree [tree]
  (if (nil? tree)
    {:length 0 :lines 0 :sentinel? true}
    tree))

(defn- leaf? [tree]
  (and (-> tree :left nil?)
       (-> tree :right nil?)))

(defn- visit-by [key f init]
  (fn [root val]
    (loop [tree root
           val val
           data init]
      (let [lval (-> tree :left key (or 0))]
        (cond
          (leaf? tree)
          (f tree nil data)
          (< val lval)
          (recur (tree :left) val (f tree :left data))
          :else
          (recur (tree :right) (- val lval) (f tree :right data)))))))

(defn- range-by [key]
  (visit-by key (fn [tree child data]
                  (condp = child
                    :left data
                    :right (-> tree :left :length (or 0) (+ data))
                    [data (-> tree :length (+ data))])) 0))

(defn- line-length [linenum]
  (visit-by :lines (fn [tree child _]
                     (if (nil? child)
                       (tree :length))) 0))

(def range-by-pos
  (range-by :length))

(defn- empty-node? [tree]
  (-> tree :length (or 0) zero?))

(defn- update-node-data [tree]
  (let [left (tree :left)
        right (tree :right)]
    (assoc tree
           :length (+ (left :length)
                      (right :length))
           :lines (+ (left :lines)
                     (right :lines)))))

(defn- rotate [tree leftkey rightkey]
  (update-node-data
    (assoc (tree leftkey) rightkey
           (update-node-data
             (update tree leftkey rightkey)))))

(defn- set-weight [tree]
  (cond
    (leaf? tree) (dissoc tree :weight)
    (-> tree :weight nil?) (assoc tree :weight (rand))
    :else tree))

(defn- weight> [a b]
  (> (-> a :weight (or -1))
     (-> b :weight (or -1))))

(defn- update-node [tree]
  (let [remove-empty (fn [tree child]
                       (if (and (-> tree child empty-node?)
                                (-> tree child :sentinel? not)) ;don't delete sentinel
                         (dissoc tree child) tree))
        parent (-> tree
                   (remove-empty :left)
                   (remove-empty :right)
                   set-weight)
        {left :left
         right :right} parent]
    (cond
      (leaf? parent) parent
      (nil? left) right
      (nil? right) left
      (weight> left parent)
      (rotate parent :left :right)
      (weight> right parent)
      (rotate parent :right :left)
      :else (update-node-data parent))))

(defn- update-by [key f]
  (fn upd [tree val]
    (update-node
      (let [lval (-> tree :left key)]
        (cond
          (leaf? tree)
          (f tree val)
          (< val lval)
          (update tree :left upd val)
          :else
          (update tree :right upd (- val lval)))))))

(defn- update-by-pos [tree pos f]
  ((update-by :length f) tree pos))

(defn- update-by-linenum [tree linenum f]
  ((update-by :lines f) tree linenum))

(def pos-linenum
  (visit-by :length (fn [tree child data]
                      (if (= child :right)
                        (-> tree :left :lines (or 0) (+ data))
                        data)) 0))

(defn- insert-line [tree pos len newline?]
  (update-by-pos tree pos
                 (fn [tree offset]
                   (cond
                     (-> tree :sentinel?)
                     {:left {:length len :lines 1}
                      :right tree}
                     newline?
                     {:left {:length (+ offset len) :lines 1}
                      :right {:length (-> tree :length (- offset)) :lines 1}}
                     :else
                     {:length (-> tree :length (+ len))
                      :lines 1}))))

(defn- insert [tree pos s]
  (loop [tree tree
         [line & lines] (split-lines s)
         pos pos]
    (if (some? line)
      (let [len (count line)]
        (recur (insert-line tree pos len (= (last line) \newline))
               lines
               (+ pos len)))
      tree)))

(defn test-insert []
  (pprint
    (-> (make-tree nil)
        (insert 0 "11\n1\n")
        (insert 0 "11\n1\n"))))

(defn- delete-line [tree pos len]
  (if (zero? len)
    tree
    (update-by-pos
      tree pos
      (fn [tree _]
        (cond
          (tree :sentinel?) ;out of range
          tree
          (> (tree :length) len)
          {:length (-> tree :length (- len)) :lines 1}
          :else
          {:length 0 :lines 0})))))

(defn- delete [tree pos len]
  ;(println "delete" pos len)
  ;(pprint tree)
  (loop [i (+ pos len)
         tree tree]
    (if (> i pos)
      (let [[a _] (range-by-pos tree (dec i))
            len (- i a)]
        (recur a (if (> pos a)
                   (-> tree
                       (delete-line a len)
                       (insert-line a (- pos a) false))
                   (delete-line tree a len))))
      tree)))

(defn create-lineindex [s]
  (insert (make-tree nil) 0 s))

(defn update-lineindex [tree {pos :pos len :len to :to}]
  (-> tree
      make-tree
      (delete pos len)
      (insert pos to)))

(defn total-lines [tree]
  (-> tree make-tree :lines))

(defn total-length [tree]
  (-> tree make-tree :length))

(def range-by-line
  (range-by :lines))

(defn pos-xy [lidx pos]
  (if (>= pos (total-length lidx))
    [0 (total-lines lidx)]
    [(- pos (first (range-by-pos lidx pos)))
     (pos-linenum lidx pos)]))

(comment defn test-pos-linenum []
         (comment pos-linenum (make-tree nil) 0)
         (comment pos-linenum (update-node
                                {:left {:length 2 :lines 1}
                                 :right nil}) 2)
         (comment pos-linenum (update-node
                                {:left {:length 2 :lines 1}
                                 :right {:length 2 :lines 1}}) 4)
         (pos-linenum (update-node
                        {:left (update-node
                                 {:left {:length 3 :lines 1}
                                  :right {:length 2 :lines 1}})
                         :right {:length 2 :lines 1}}) 5))

(comment defn test-line-pos-range []
         (range-by-line (update-node
                          {:left (update-node
                                   {:left {:length 3 :lines 1}
                                    :right {:length 2 :lines 1}})
                           :right {:length 2 :lines 1}}) 3))

(defn test-file []
  (comment pprint (create-lineindex (str (slurp "/Users/roy/webvim/src/webvim/core/lineindex.clj") \newline)))
  (pprint (create-lineindex "")))
