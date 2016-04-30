(ns webvim.core.lineindex
  "log(n) time get line number from pos or get pos range from line number"
  (:require [clojure.string :as string]
            [webvim.core.utils :refer [negzero split-lines]]
            [clojure.pprint :refer [pprint]]))

(defn- make-tree [tree]
  (if (nil? tree)
    {:length 0 :lines 0}
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

(defn- update-node [tree]
  (let [remove-empty (fn [tree child]
                       (if (-> tree child empty-node?)
                         (dissoc tree child) tree))
        tree (-> tree
                 (remove-empty :left)
                 (remove-empty :right))
        {priority :priority
         left :left
         right :right} tree]
    (cond
      (leaf? tree) tree
      (nil? left) right
      (nil? right) left
      (-> left :priority (or -1) (> priority))
      (update-node-data
        {:left (left :left)  ;alpha: (left :left); beta: (left :right); lambda: right
         :right (update-node-data
                  {:left (left :right)
                   :right right
                   :priority (tree :priority)})
         :priority (left :priority)})
      (-> right :priority (or -1) (> priority))
      (update-node-data ;alpha: left; beta: (right :left); lambda: (right :right)
        {:left (update-node-data
                 {:left left
                  :right (right :left)
                  :priority (tree :priority)})
         :right (right :right)
         :priority (right :priority)})
      :else (update-node-data tree))))

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

(defn- delete [tree pos len]
  (let [[a b] (range-by-pos tree pos)
        tree (update-by-pos tree pos
                            (fn [tree offset]
                              (let [newlen (-> tree :length
                                               (- offset len)
                                               negzero)
                                    newlines (if (zero? newlen) 0 1)]
                                {:length newlen
                                 :lines newlines})))
        dlen (- b pos)]
    (if (< len dlen)
      tree
      (recur tree pos (- len dlen)))))

(comment defn test-delete []
         (comment pprint (delete (update-node
                                   {:left (update-node
                                            {:left {:length 3 :lines 1}
                                             :right {:length 2 :lines 1}})
                                    :right {:length 2 :lines 1}})
                                 3 1))
         (pprint (delete (update-node
                           {:left (update-node
                                    {:left {:length 3 :lines 1}
                                     :right {:length 2 :lines 1}})
                            :right {:length 2 :lines 1}})
                         3 3)))

(defn- insert [tree pos s]
  (loop [tree tree
         [line & lines] (split-lines s)
         pos pos]
    (if (nil? line)
      tree
      (let [len (count line)]
        (recur (update-by-pos tree pos
                              (fn [tree offset]
                                (cond
                                  (-> tree :length (= offset)) ;last line MUST contains \newline
                                  {:left tree
                                   :right {:length len :lines 1}
                                   :priority (rand)}
                                  (= (last line) \newline)
                                  {:left {:length (+ offset len) :lines 1}
                                   :right {:length (-> tree :length (- offset)) :lines 1}
                                   :priority (rand)}
                                  :else
                                  {:lines 1
                                   :length (-> tree :length (+ len))})))
               lines (+ pos len))))))

(defn test-insert []
  (pprint
    (-> (make-tree nil)
        (insert 0 "11\n1\n")
        (insert 0 "11\n1\n"))))

(defn create-lineindex [s]
  (insert (make-tree nil) 0 s))

(defn update-lineindex [tree {pos :pos len :len to :to}]
  (-> tree
      make-tree
      (delete pos len)
      (insert pos to)))

(defn total-lines [tree]
  (-> tree make-tree :lines))

(def pos-linenum
  (visit-by :length (fn [tree child data]
                      (if (= child :right)
                        (-> tree :left :lines (or 0) (+ data))
                        data)) 0))

(def range-by-line
  (range-by :lines))

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
  (pprint (create-lineindex (str (slurp "/Users/roy/webvim/src/webvim/core/lineindex.clj") \newline))))
