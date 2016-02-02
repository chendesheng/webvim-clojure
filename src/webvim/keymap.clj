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
        webvim.keymap.linebuf.linebuf
        webvim.keymap.pair
        webvim.keymap.action
        webvim.keymap.showkeys
        webvim.keymap.dotrepeat
        webvim.keymap.autocompl
        webvim.keymap.register
        webvim.core.register
        webvim.jumplist
        webvim.core.utils
        webvim.core.event
        webvim.indent
        webvim.autocompl
        webvim.keymap.compile))

(defn init-keymap-tree []
  (let [motion-keymap (init-motion-keymap)
        normal-mode-keymap (fire-event
                             (init-normal-mode-keymap motion-keymap)
                             :normal-mode-keymap)
        insert-mode-keymap (fire-event
                             (init-insert-mode-keymap normal-mode-keymap)
                             :insert-mode-keymap)
        ex-mode-keymap (fire-event
                         (init-ex-mode-keymap)
                         :ex-mode-keymap)]
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
