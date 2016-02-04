(ns webvim.keymap.repeat
  (:use webvim.core.pos
        webvim.core.buffer
        webvim.core.register
        webvim.core.lang
        webvim.core.rope
        webvim.core.line
        webvim.core.event
        webvim.core.ui
        webvim.core.parallel-universe
        webvim.keymap.linebuf.linebuf
        webvim.keymap.action
        webvim.keymap.ex
        webvim.jumplist
        webvim.core.utils))

(defn- reset-repeat-prefix[buf keycode]
  (if (and
        (-> buf :context :repeat-prefix nil? not)
        (not (re-test #"^[0-9]$" keycode)))
    (update-in buf [:context] dissoc :repeat-prefix)
    buf))

(defn- on-normal-mode-keymap[keymap]
  (-> keymap
      (wrap-key "0" (fn[handler]
                      (fn[buf keycode]
                        (if (-> buf :context :repeat-prefix nil? not)
                          (append-repeat-prefix buf "0")
                          (handler buf)))))
      (wrap-key :else (fn[handler]
                        (fn[buf keycode]
                          (if (re-test #"^[0-9]$" keycode)
                            (append-repeat-prefix buf keycode)
                            buf))))
      (wrap-key :after (fn[handler]
                         (fn[buf keycode]
                           (-> buf
                               (handler keycode)
                               (reset-repeat-prefix keycode)))))))

(defonce ^:private listener1
  (listen :normal-mode-keymap
          (fn[keymap]
            (on-normal-mode-keymap keymap))))
