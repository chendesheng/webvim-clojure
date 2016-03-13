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
            [webvim.visual :refer [set-visual-mode]]
            [webvim.mode :refer [set-normal-mode ex-mode insert-mode normal-mode]]
            [webvim.keymap.motion :refer [init-motion-keymap init-motion-keymap-with-objects]]
            [webvim.keymap.replace :refer [wrap-keymap-replace]])
  (:use clojure.pprint
        webvim.keymap.action
        webvim.keymap.macro
        webvim.keymap.insert
        webvim.keymap.ex
        webvim.keymap.visual
        webvim.core.buffer
        webvim.core.rope
        webvim.core.utils))

(defn- normal-mode-after [buf keycode]
  (let [insert-mode? (= (buf :mode) insert-mode)
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
          :mode ex-mode))))

(defn init-normal-mode-keymap [buf]
  (let [motion-keymap (init-motion-keymap)
        visual-mode-keymap (init-visual-mode-keymap-with-operators
                             (init-motion-keymap-with-objects) buf)]
    (-> (deep-merge
          motion-keymap
          {":" start-ex-mode
           "u" (wrap-keycode undo)
           "<c-r>" (wrap-keycode redo)
           "<c-g>" (wrap-keycode buf-pos-info)
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
        wrap-keymap-change
        wrap-keymap-case)))

