(ns webvim.keymap
  (:use clojure.pprint
        webvim.core.event
        webvim.core.rope
        webvim.core.pos
        webvim.core.buffer
        webvim.core.line
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

(defn init-keymap-tree
  []
  (let [insert-mode-keymap (init-insert-mode-keymap)
        pair-keymap (init-pair-keymap)
        line-editor-keymap (init-line-editor-keymap)
        motion-keymap (init-motion-keymap line-editor-keymap)
        ex-mode-keymap (init-ex-mode-keymap line-editor-keymap)
        visual-mode-keymap (init-visual-mode-keymap motion-keymap pair-keymap visual-normal)
        visual-line-mode-keymap (init-visual-mode-keymap motion-keymap pair-keymap visual-line)
        normal-mode-keymap (init-normal-mode-keymap motion-keymap insert-mode-keymap visual-mode-keymap visual-line-mode-keymap ex-mode-keymap pair-keymap)]
    (reset! root-keymap (compile-keymap normal-mode-keymap))))

(defonce ^:private listen-new-buffer
  (listen :new-buffer
          (fn [buf]
            (assoc buf :root-keymap @root-keymap))))


;(test-keymap)
;(pprint
;  ((compile-keymap @root-keymap) ":else:elsetc"))

(comment
  (defn- print2[buf]
    (println (buf :keys))
    buf)

  (defn test-keymap[]
    (str ((let [buf (assoc (open-file nil) :root-keymap (init-keymap-tree)) ]
            (-> buf
                (apply-keycode "/")
                (apply-keycode "w")
                (print2)
                (apply-keycode "a"))) :str))))
