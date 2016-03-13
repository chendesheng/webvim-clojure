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
            [webvim.keymap.change :refer [wrap-keymap-change]]
            [webvim.keymap.visual :refer [wrap-keymap-visual update-x-if-not-jk]]
            [webvim.mode :refer [set-normal-mode]]
            [webvim.keymap.motion :refer [init-motion-keymap init-motion-keymap-with-objects]]
            [webvim.core.pos :refer [char-]]
            [webvim.core.event :refer [listen]]
            [webvim.keymap.compile :refer [wrap-keycode]]
            [webvim.keymap.replace :refer [wrap-keymap-replace]])
  (:use clojure.pprint
        webvim.keymap.insert
        webvim.keymap.ex
        webvim.core.buffer
        webvim.core.rope
        webvim.core.utils))

(defn- normal-mode-fix-pos
  "prevent cursor on top of EOL in normal mode"
  [buf]
  (let [ch (char-at (buf :str) (buf :pos))]
    (if (= (or ch \newline) \newline)
      (char- buf) buf)))

(defn- normal-mode-after [buf keycode]
  (let [insert-mode? (= (buf :mode) :insert-mode)
        lastbuf (-> buf :context :lastbuf)
        save-undo (if insert-mode? identity save-undo)
        fix-pos (if insert-mode? identity normal-mode-fix-pos)]
    ;(if-not (nil? (motions-push-jumps (string/join (buf :keys))))
    ;  (jump-push lastbuf))
    (-> buf
        fix-pos
        (update-x-if-not-jk keycode)
        (update-in [:context] dissoc :range)
        save-undo)))

(defn- start-ex-mode [buf keycode]
  (let [enter (or (-> buf :ex-mode-keymap :enter) nop)]
    (-> buf
        (enter keycode)
        (assoc 
          :keymap (buf :ex-mode-keymap)
          :mode :ex-mode))))

(defn init-normal-mode-keymap [buf]
  (-> (init-motion-keymap) 
      (merge
        {":" start-ex-mode
         "u" (wrap-keycode undo)
         "<c-r>" (wrap-keycode redo)
         "<c-g>" (wrap-keycode buf-pos-info)
         "<esc>" (wrap-keycode set-normal-mode)
         :continue (fn [buf keycode]
                     (= (buf :mode) :normal-mode))
         :before (fn [buf keycode]
                   (-> buf
                       (assoc-in [:context :lastbuf] buf)
                       (assoc-in [:context :range] nil)))
         :after normal-mode-after})
      (wrap-keymap-visual buf)
      wrap-keymap-addsub
      wrap-keymap-indent
      wrap-keymap-replace
      wrap-keymap-scrolling
      wrap-keymap-yank
      wrap-keymap-delete
      wrap-keymap-join
      wrap-keymap-put
      wrap-keymap-jump
      wrap-keymap-change
      wrap-keymap-case))

(listen :before-change-to-normal-mode
        normal-mode-fix-pos)
