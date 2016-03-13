(ns webvim.keymap.normal
  (:require [clojure.string :as string]
            [webvim.keymap.scrolling :refer [wrap-keymap-scrolling]]
            [webvim.keymap.indent :refer [wrap-keymap-indent]]
            [webvim.keymap.case :refer [wrap-keymap-case]]
            [webvim.keymap.addsub :refer [wrap-keymap-addsub]]
            [webvim.keymap.yank :refer [wrap-keymap-yank]]
            [webvim.keymap.delete :refer [wrap-keymap-delete]]
            [webvim.keymap.join :refer [wrap-keymap-join]]
            [webvim.keymap.put :refer [wrap-keymap-put]]
            [webvim.keymap.jump :refer [wrap-keymap-jump]]
            [webvim.keymap.replace :refer [wrap-keymap-replace]])
  (:use clojure.pprint
        webvim.keymap.action
        webvim.keymap.macro
        webvim.keymap.motion
        webvim.keymap.insert
        webvim.keymap.ex
        webvim.keymap.visual
        webvim.core.buffer
        webvim.core.rope
        webvim.core.line
        webvim.core.pos
        webvim.core.register
        webvim.indent
        webvim.core.utils
        webvim.jumplist))

(defn- delete-char [buf]
  (let [pos (buf :pos)
        [a b] [pos (inc pos)]]
    (buf-yank buf a b false true)
    (buf-delete buf a b)))

(defn- delete-line [buf]
  (let [pos (buf :pos)
        r (buf :str)
        [a b] (pos-line r pos)]
    (-> buf
        (buf-yank a b false true)
        (buf-replace a (dec b) "")
        buf-indent-current-line)))

(defn- insert-new-line [buf]
  (buf-indent-current-line
    (let [pos (buf :pos)
          r (buf :str)
          b (pos-line-last r pos)]
      (-> buf
          (buf-insert b <br>)
          (buf-set-pos b)))))

(defn- insert-new-line-before [buf]
  (buf-indent-current-line
    (let [pos (buf :pos)
          r (buf :str)
          a (pos-line-first r pos)]
      (if (zero? a)
        (-> buf
            (buf-insert 0 <br>)
            buf-start)
        (-> buf
            (buf-set-pos (- a 1))
            (buf-insert <br>))))))

(defn- pos-info [buf]
  (let [{y :y
         x :x
         linescnt :linescnt} buf
        percent (-> y inc (* 100) (/ linescnt) int)]
    (assoc buf :message (format "\"%s\" line %d of %d --%d%%-- col %d" 
                                (printable-filepath buf)
                                (inc y) linescnt percent (inc x)))))

(defn- inclusive? [keycode]
  (println "keycode:" keycode)
  (let [m {"h" false "l" false "w" false "W" false "e" true
           "E" true "b" false "B" false "f" true "F" false
           "t" true "T" false "/" false "$" false "a" true
           "i" true}]
    (if (contains? m keycode)
      (m keycode)
      true)))

;setup range prefix for delete/change/yank etc.
(defn- setup-range [buf]
  (println "setup-range:" (-> buf :context :range))
  (if (-> buf :context :range nil?)
    (let [pos (buf :pos)
          lastbuf (-> buf :context :lastbuf)
          lastpos (lastbuf :pos)]
      (-> buf
          ;Restore positoin to lastbuf so that changes happen next can record correct start position. This will make cursor position in right place after undo/redo.
          (merge (select-keys lastbuf [:pos :x :y]))
          (assoc-in [:context :range] [lastpos pos]))) buf))

(defn- setup-range-line [buf]
  (assoc-in buf [:context :range] (pos-line (buf :str) (buf :pos))))

(defn- setup-range-line-end [buf]
  (let [a (buf :pos)
        b (pos-line-end (buf :str) a)]
    (assoc-in buf [:context :range] [a b])))

(defn- normal-mode-after [buf keycode]
  (let [insert-mode? (= (buf :mode) insert-mode)
        lastbuf (-> buf :context :lastbuf)
        save-undo (if insert-mode? identity save-undo)]
    (if-not (nil? (motions-push-jumps (string/join (buf :keys))))
      (jump-push lastbuf))
    (let [newbuf (if insert-mode? buf
                     (normal-mode-fix-pos buf))]
      (-> newbuf
          (update-x-if-not-jk keycode)
          (update-in [:context] dissoc :range)
          save-undo))))

(defn- start-insert-mode-with-keycode [fnmotion fnedit]
  (fn [buf keycode]
    (-> buf 
        (fnmotion keycode)
        set-insert-mode
        (fnedit keycode))))

(defn- start-ex-mode [buf keycode]
  (let [enter (or (-> buf :ex-mode-keymap :enter) nop)]
    (-> buf
        (enter ":")
        (assoc 
          :keymap (buf :ex-mode-keymap)
          :mode ex-mode))))

(defn- change-to-line-end [buf]
  (-> buf
      setup-range-line-end
      (change-range false false)))

(defn- change-by-motion [buf keycode]
  (-> buf 
      setup-range
      (change-range (inclusive? keycode) false)))

(defn init-normal-mode-keymap [buf]
  (let [motion-keymap (init-motion-keymap)
        visual-mode-keymap (init-visual-mode-keymap-with-operators
                             (init-motion-keymap-with-objects) buf)
        motion-keymap-fix-w (init-motion-keymap-for-operators)
        motion-keymap-fix-cw (init-motion-keymap-fix-cw)]
    (-> (deep-merge
          motion-keymap
          {"i" (start-insert-mode identity identity)
           "a" (start-insert-mode char+ identity)
           "A" (start-insert-mode line-end identity)
           "I" (start-insert-mode line-start identity)
           "s" (start-insert-mode identity delete-char)
           "S" (start-insert-mode identity delete-line)
           "o" (start-insert-mode identity insert-new-line)
           "O" (start-insert-mode identity insert-new-line-before)
           ":" start-ex-mode
           "u" (wrap-keycode undo)
           "<c-r>" (wrap-keycode redo)
           "<c-g>" (wrap-keycode pos-info)
           "<esc>" (wrap-keycode set-normal-mode)
           "g" {"v" (assoc
                      visual-mode-keymap
                      :enter
                      (fn [buf keycode]
                        (let [visual (buf :last-visual)]
                          (-> buf
                              (set-visual-mode visual)
                              (buf-set-pos (-> visual :range first))))))}
           "v" visual-mode-keymap
           "V" visual-mode-keymap
           "<c-v>" visual-mode-keymap
           "c" (merge
                 motion-keymap-fix-cw
                 {"c" (start-insert-mode identity delete-line)
                  :after (fn [buf keycode]
                           (if (or (= keycode "c")
                                   (and
                                     (= (-> buf :context :lastbuf :pos) (buf :pos))
                                     (-> buf :context :range empty?)))
                             buf
                             ((start-insert-mode-with-keycode nop change-by-motion) buf keycode)))})
           "C" (start-insert-mode identity change-to-line-end)
           :continue (fn [buf keycode]
                       (= (buf :mode) normal-mode))
           :before (fn [buf keycode]
                     (-> buf
                         (assoc-in [:context :lastbuf] buf)
                         (assoc-in [:context :range] nil)))
           :after normal-mode-after})
        wrap-keymap-addsub
        wrap-keymap-indent
        wrap-keymap-replace
        wrap-keymap-scrolling
        wrap-keymap-yank
        wrap-keymap-delete
        wrap-keymap-join
        wrap-keymap-put
        wrap-keymap-jump
        wrap-keymap-case)))

