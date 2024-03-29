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
            [webvim.keymap.visual :refer [wrap-keymap-visual update-x-if-not-jk init-visual-mode-keymap-for-operators]]
            [webvim.mode :refer [set-normal-mode]]
            [webvim.keymap.motion :refer [init-motion-keymap init-motion-keymap-with-objects]]
            [webvim.core.pos :refer [char-]]
            [webvim.core.event :refer [listen]]
            [webvim.core.line :refer [buf-update-column]]
            [webvim.jumplist :refer [motions-push-jumps jump-push cursor-location]]
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
        save-undo (if insert-mode? identity save-undo)
        fix-pos (if insert-mode? identity normal-mode-fix-pos)
        save-location (if (some? (motions-push-jumps (string/join (buf :keys))))
                        (let [loc (-> buf :context :lastbuf cursor-location)]
                          #(jump-push % loc))
                        identity)]

    (-> buf
        save-location
        fix-pos
        (update-x-if-not-jk keycode)
        (update :context dissoc :range)
        (update :context dissoc :motion-fail?)
        (update :context dissoc :motion-cancel?)
        save-undo)))

(defn init-normal-mode-keymap [buf]
  (let [visual-keymap (init-visual-mode-keymap-for-operators)]
    (-> (init-motion-keymap)
        (merge
          {"u" (wrap-keycode undo)
           "<c-r>" (wrap-keycode redo)
           "<c-g>" (wrap-keycode buf-pos-info)
           "<esc>" (wrap-keycode set-normal-mode)
           :continue (fn [buf keycode]
                       (= (buf :mode) :normal-mode))
           :before (fn [buf keycode]
                     (update buf :context assoc :lastbuf buf))
           :after normal-mode-after})
        (wrap-keymap-visual buf)
        wrap-keymap-addsub
        (wrap-keymap-indent visual-keymap)
        wrap-keymap-replace
        wrap-keymap-scrolling
        (wrap-keymap-yank visual-keymap)
        (wrap-keymap-delete visual-keymap)
        wrap-keymap-join
        wrap-keymap-put
        wrap-keymap-jump
        (wrap-keymap-change visual-keymap)
        (wrap-keymap-case visual-keymap))))

(listen :before-change-to-normal-mode normal-mode-fix-pos)
