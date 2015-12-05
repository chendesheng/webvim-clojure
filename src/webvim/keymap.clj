(ns webvim.keymap
  (:require [clojure.core.async :as async])
  (:use clojure.pprint
        webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.buffer
        webvim.core.serve
        webvim.core.line
        webvim.keymap.motion
        webvim.keymap.visual
        webvim.keymap.normal
        webvim.keymap.insert
        webvim.keymap.ex
        webvim.keymap.line-editor
        webvim.keymap.pair
        webvim.keymap.action
        webvim.core.register
        webvim.jumplist
        webvim.utils
        webvim.indent
        webvim.autocompl))


(defn- tree-reduce-recur[visit-branch visit-leaf path ctx root]
  (reduce 
    (fn[ctx [k v :as node]]
      (let [path (conj path node)]
        (if (map? v)
          (visit-branch 
            (tree-reduce-recur 
              visit-branch visit-leaf path ctx v) path)
          (visit-leaf ctx path)))) ctx root))

;deep first order
(defn- tree-reduce[visit-branch visit-leaf ctx root]
  (tree-reduce-recur visit-branch visit-leaf (seq {"" root}) ctx root))

;(tree-reduce 
;  (fn[ctx path]
;    ;(println (first path))
;    ctx)
;  (fn[ctx path]
;    (str ctx (-> path first val))) "" {:a {:aa "haha" :c {:cc "ccc"} } :b "hehe"})


(defn- nop[buf keycode]
  buf)

(defn- stop[buf keycode]
  false)

(defn- compile-keymap[keymap]
  (tree-reduce
    (fn[ctx [[_ {enter :enter}] [_ {before :before}] & _ :as path]]
      (assoc ctx 
             (clojure.string/join (map key path))
             `(~#(update-in %1 [:keys] conj %2) ~enter ~before)))
    (fn[ctx [[keycode func] & [[_ {before :before after :after continue? :continue}] & _ :as allparents] :as path]]
      (if (contains? #{:enter :leave :before :after :continue} keycode)
        ctx
        (let [func (if (= keycode :else)
                     func
                     (fn[buf keycode]
                       (func buf)))
              funcs `(~func ~before)]
          (assoc ctx
                 (clojure.string/join (map key path))
                 (conj funcs 
                       (fn[buf keycode]
                         (if (empty? allparents) buf
                           (let [buf (update-in buf [:keys] conj keycode)]
                             ;(println "parents" allparents)
                             (reduce
                               (fn[buf [_ {after :after continue? :continue leave :leave}]]
                                 (let [keycode (-> buf :keys first)
                                       after (or after nop)
                                       continue? (or continue? stop)
                                       leave (or leave nop)
                                       buf1 (after buf keycode)]
                                   ;;(println "keys:" (buf1 :keys))
                                   (if (continue? buf1 keycode)
                                     (-> buf1
                                         (update-in [:keys] pop)
                                         reduced)
                                     (-> buf1
                                         (update-in [:keys] pop)
                                         (leave keycode)))))
                               buf allparents)))))))))
    {}
    keymap))

(defn- keymap-comp[funcs]
  (let [funcs (filter (comp not nil?) funcs)]
    ;(pprint funcs)
    (if (empty? funcs)
      nil
      (fn[buf keycode]
        (reduce 
          (fn[buf f]
            (f buf keycode)) buf (reverse funcs))))))

(defn apply-keycode[buf keycode]
  (let [keymap (buf :root-keymap)
        allkeycode (conj (buf :keys) keycode)
        ;_ (println (buf :keys))
        _ (println allkeycode)
        ;_ (pprint (apply comp (filter (comp not nil?) (actions (clojure.string/join (conj (buf :keys) ":else"))))))
        func (or (keymap-comp (keymap (clojure.string/join allkeycode))) 
                 (keymap-comp (keymap (clojure.string/join (conj (buf :keys) ":else"))))
                 nop)]
    (func buf keycode)))

(defonce root-keymap (atom {}))

(defn init-keymap-tree
  []
  (let [insert-mode-keymap (init-insert-mode-keymap)
        pair-keymap (init-pair-keymap)
        line-editor-keymap (init-line-editor-keymap)
        motion-keymap (init-motion-keymap line-editor-keymap)
        ex-mode-keymap (init-ex-mode-keymap motion-keymap line-editor-keymap)
        visual-mode-keymap (init-visual-mode-keymap motion-keymap pair-keymap visual-normal)
        visual-line-mode-keymap (init-visual-mode-keymap motion-keymap pair-keymap visual-line)
        normal-mode-keymap (init-normal-mode-keymap motion-keymap insert-mode-keymap visual-mode-keymap visual-line-mode-keymap ex-mode-keymap pair-keymap)]
    (reset! root-keymap (compile-keymap normal-mode-keymap))))

(defn- buf-bound-scroll-top
  "Change scroll top make cursor inside viewport"
  [buf]
  (let [st (buf :scroll-top)]
    (assoc buf :scroll-top 
           (let [y (buf :y)
                 h (-> @window :viewport :h)]
             (cond 
               (< y st) y
               (< y (+ st h)) st
               (neg? (-> y (- h) inc)) 0
               :else (-> y (- h) inc))))))

(defonce ^:private listen-new-buffer
  (listen :new-buffer
          (fn [buf]
            (assoc buf :root-keymap @root-keymap))))

(defn test-keymap[]
  (str ((let [buf (assoc (open-file nil) :root-keymap (init-keymap-tree)) ]
          (-> buf
              (apply-keycode "i")
              (apply-keycode "a")
              (apply-keycode "a")
              (apply-keycode "a")
              (apply-keycode "<esc>")
              (apply-keycode "h")
              (apply-keycode "h"))) :pos)))

;(test-keymap)
;(pprint 
;  ((compile-keymap @root-keymap) "h"))
