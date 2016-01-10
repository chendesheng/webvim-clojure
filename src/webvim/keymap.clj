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
        visual-mode-keymap (init-visual-mode-keymap motion-keymap pair-keymap)
        normal-mode-keymap (init-normal-mode-keymap motion-keymap visual-mode-keymap pair-keymap)
        insert-mode-keymap (init-insert-mode-keymap)
        ex-mode-keymap (init-ex-mode-keymap line-editor-keymap)]
    {:normal-mode-keymap normal-mode-keymap
     :insert-mode-keymap insert-mode-keymap
     :ex-mode-keymap ex-mode-keymap}))

(listen :new-buffer
        (fn[buf]
          (let [tmp (@ui-agent :keymaps)
                keymaps (or tmp (init-keymap-tree))]
            (if (nil? tmp)
                  (send ui-agent
                        (fn[ui]
                          (assoc ui :keymaps keymaps))))
            (merge buf 
                   (assoc keymaps 
                          :keymap (keymaps :normal-mode-keymap))))))
