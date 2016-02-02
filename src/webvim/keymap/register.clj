(ns webvim.keymap.register
  (:use clojure.pprint
        webvim.keymap.action
        webvim.keymap.linebuf.linebuf
        webvim.core.ui
        webvim.core.rope
        webvim.core.register
        webvim.core.buffer
        webvim.core.event))

(defn- start-register[buf keycode]
  (if (re-test #"[0-9a-zA-Z/*#%.:+=\-_~]" keycode)
    (assoc-in buf [:context :register] keycode)
    buf))

(defn- read-eval-put[buf code insert?]
  (try
    (let [result (->> code read-string eval str)]
      (registers-put! "=" {:str code :result result})
      (if insert?
        (-> buf
            (assoc-in [:context :register] "=")
            (buf-insert result))
        (assoc-in buf [:context :register] "=")))
    (catch Exception e
      (assoc buf :message (str e)))))

(defonce ^:private linebuf-keymap (init-linebuf-keymap))
(defn- expression-keymap[insert?]
  (-> linebuf-keymap
      (wrap-key 
        :enter (fn[handler]
                 (fn[buf keycode]
                   (-> buf
                       (assoc :line-buffer {:prefix keycode :str (rope "()") :pos 1})
                       (handler keycode)))))
      (assoc "<cr>" (fn[buf] 
                      (let [code (-> buf :line-buffer :str str)]
                        (read-eval-put buf code insert?))))))

(defn- on-normal-mode-keymap[keymap]
  (-> keymap
      (wrap-key :before
                (fn[handler]
                  (fn[buf keycode]
                    (-> buf
                        (update-in [:context :register]
                                   (fn[reg]
                                     (or reg "\"")))
                        (handler keycode)))))
      (assoc "\"" {"<esc>" identity
                   "=" (expression-keymap false)
                   :else start-register})))

(defn- on-insert-mode-keymap[keymap]
  (-> keymap
      (update-in ["<c-r>"] assoc "=" (expression-keymap true))))

(defonce ^:private listener1
  (listen
    :normal-mode-keymap
    (fn[keymap]
      (on-normal-mode-keymap keymap))))

(defonce ^:private listener2
  (listen
    :insert-mode-keymap
    (fn[keymap]
      (on-insert-mode-keymap keymap))))

