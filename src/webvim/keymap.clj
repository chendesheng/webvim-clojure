(ns webvim.keymap
  (:require [clojure.core.async :as async])
  (:use clojure.pprint
        webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.buffer
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


(defn- record-keys[buf keycode]
  (if (nil? (#{"c+n" "c+p" "c+a" "c+x"} keycode)) ;Don't record keycode for these commands
    (update-in buf [:macro :recording-keys] conj keycode)
    buf))

(defn- nop[buf keycode]
  buf)

(defn- stop[buf keycode]
  false)

(defn- compile-keymap[keymap]
  (tree-reduce
    (fn[ctx [[_ {enter :enter}] [_ {before :before}] & _ :as path]]
      (assoc ctx
             (clojure.string/join (map key path))
             `(~#(update-in %1 [:keys] conj %2) ~enter ~before ~record-keys)))
    (fn[ctx [[keycode func] & [[_ {before :before after :after continue? :continue}] & _ :as allparents] :as path]]
      (if (contains? #{:enter :leave :before :after :continue} keycode)
        ctx
        (let [func (if (= keycode :else)
                     func
                     (fn[buf keycode]
                       (func buf)))
              funcs `(~func ~before ~record-keys)]
          ;(println "keycode:" keycode)
          (assoc ctx
                 (clojure.string/join (map key path))
                 (conj funcs
                       (fn[buf keycode]
                         ;(println "keycode:" keycode)
                         (if (empty? allparents) buf
                           (let [buf (update-in buf [:keys] conj keycode)]
                             (println "keys:" (buf :keys))
                             (reduce
                               (fn[buf [_ {after :after continue? :continue leave :leave}]]
                                 (let [keycode (-> buf :keys first)
                                       after (or after nop)
                                       continue? (or continue? stop)
                                       leave (or leave nop)
                                   _ (println "keys:3" (buf :keys))
                                       buf1 (after buf keycode)]
                                   (println "keys:2" (buf1 :keys))
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
        ;_ (println allkeycode)
        func (or (keymap-comp 
                   (or (keymap (clojure.string/join allkeycode))
                       (keymap (clojure.string/join (conj (buf :keys) ":else")))
                       (if (-> buf :keys empty? not)
                         (or
                          (keymap (clojure.string/join (conj (pop (buf :keys)) ":else" keycode))) ;last :else can be map too
                          (keymap (clojure.string/join (conj (pop (buf :keys)) ":else:else")))))))
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

(defn- print2[buf]
  (println (buf :keys))
  buf)

(defn test-keymap[]
  (str ((let [buf (assoc (open-file nil) :root-keymap (init-keymap-tree)) ]
          (-> buf
              (apply-keycode "/")
              (apply-keycode "w")
              (print2)
              (apply-keycode "a"))) :str)))

;(test-keymap)
;(pprint 
;  ((compile-keymap @root-keymap) ":else:elsetc"))
