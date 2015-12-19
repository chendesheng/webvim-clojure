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
  (let [pair-keymap (init-pair-keymap)
        line-editor-keymap (init-line-editor-keymap)
        motion-keymap (init-motion-keymap line-editor-keymap)
        visual-mode-keymap (init-visual-mode-keymap motion-keymap pair-keymap visual-range)
        visual-line-mode-keymap (init-visual-mode-keymap motion-keymap pair-keymap visual-line)

        normal-mode-keymap (compile-keymap (init-normal-mode-keymap motion-keymap visual-mode-keymap visual-line-mode-keymap pair-keymap))
        insert-mode-keymap (compile-keymap (init-insert-mode-keymap))
        ex-mode-keymap (compile-keymap (init-ex-mode-keymap line-editor-keymap))]
    (send ui-agent 
          (fn[ui]
            (assoc ui
              :normal-mode-keymap normal-mode-keymap
              :insert-mode-keymap insert-mode-keymap
              :ex-mode-keymap ex-mode-keymap)))
    (await ui-agent)))

(listen :new-buffer
        (fn[buf]
          (assoc buf 
            :keymap (@ui-agent :normal-mode-keymap)
            :normal-mode-keymap (@ui-agent :normal-mode-keymap)
            :insert-mode-keymap (@ui-agent :insert-mode-keymap)
            :ex-mode-keymap (@ui-agent :ex-mode-keymap))))
