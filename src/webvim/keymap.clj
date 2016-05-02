(ns webvim.keymap
  (:use webvim.core.event
        webvim.keymap.normal
        webvim.keymap.insert
        webvim.keymap.ex
        webvim.keymap.showkeys
        webvim.keymap.dotrepeat
        webvim.keymap.repeat
        webvim.keymap.autocompl
        webvim.keymap.register
        webvim.keymap.compile))

(defn init-keymap-tree [buf]
  ;Keep in mind buf argument is not grantee not nil.
  ;The listeners MUST handle nil buf case.
  (let [normal-mode-keymap (fire-event
                             :normal-mode-keymap
                             (init-normal-mode-keymap buf)
                             buf)
        insert-mode-keymap (fire-event
                             :insert-mode-keymap
                             (init-insert-mode-keymap normal-mode-keymap buf)
                             buf)]
    {:normal-mode-keymap normal-mode-keymap
     :insert-mode-keymap insert-mode-keymap}))

(listen :new-buffer
        (fn [buf]
          (let [keymaps (init-keymap-tree buf)]
            (merge buf
                   (assoc keymaps
                          :keymap (keymaps :normal-mode-keymap))))))
