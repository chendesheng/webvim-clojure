(ns webvim.keymap
  (:use clojure.pprint
        webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.buffer
        webvim.core.line
        webvim.core.ui
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
        webvim.core.utils
        webvim.indent
        webvim.autocompl
        webvim.keymap.compile))

(defn init-keymap-tree []
  (let [insert-mode-keymap (init-insert-mode-keymap)
        pair-keymap (init-pair-keymap)
        line-editor-keymap (init-line-editor-keymap)
        motion-keymap (init-motion-keymap line-editor-keymap)
        ex-mode-keymap (init-ex-mode-keymap line-editor-keymap)
        visual-mode-keymap (init-visual-mode-keymap insert-mode-keymap motion-keymap pair-keymap visual-normal)
        visual-line-mode-keymap (init-visual-mode-keymap insert-mode-keymap motion-keymap pair-keymap visual-line)
        normal-mode-keymap (init-normal-mode-keymap motion-keymap insert-mode-keymap visual-mode-keymap visual-line-mode-keymap ex-mode-keymap pair-keymap)]
    (send ui-agent 
          (fn[ui normal-mode-keymap insert-mode-keymap ex-mode-keymap]
            (assoc ui
              :normal-mode-keymap (compile-keymap normal-mode-keymap)
              :insert-mode-keymap (compile-keymap insert-mode-keymap)
              :ex-mode-keymap (compile-keymap ex-mode-keymap))) normal-mode-keymap insert-mode-keymap ex-mode-keymap)))

