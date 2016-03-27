(ns webvim.core.ui
  (:require [webvim.core.event :refer [listen]]
            [webvim.core.editor :refer [*window*]])
  (:use webvim.core.line))

(defn- dissoc-empty [buf ks]
  (if (empty? (get-in buf ks))
    (if (= 1 (count ks))
      (dissoc buf (first ks))
      (recur (update-in buf (pop ks) dissoc (peek ks)) (pop ks)))
    buf))

(defn- dissoc-nil [buf k]
  (if (nil? (buf k))
    (dissoc buf k)
    buf))

(defn- dissoc-if-equal [after before k]
  (if (and (not (nil? before))
           (= (before k) (after k)))
    (dissoc after k)
    after))

(defn- remove-autocompl [{autocompl :autocompl :as after} before]
  (if (nil? autocompl)
    after
    (assoc after :autocompl
           (-> autocompl
               (dissoc-if-equal (:autocompl before) :suggestions)
               (dissoc :words
                       :w
                       :uncomplete-word
                       :replace
                       :limit-number)))))

(defn- line-editor [buf]
  (if (nil? (buf :line-buffer))
    buf
    (let [{s :str
           prefix :prefix
           pos :pos
           pos2 :pos2} (-> buf :line-buffer)] 
      (update buf :line-buffer (fn [line-buf]
                                 (-> line-buf
                                     (dissoc :prefix)
                                     (assoc :str (str prefix s)
                                            :pos (+ pos (count prefix))
                                            :pos2 (if (nil? pos2) nil (+ pos2 (count prefix))))))))))

(defn- visual-type [type]
  ({:no-visual 0
    :visual-range 1
    :visual-line 2
    :visual-block 3} type))

(defn- buf-mode [mode]
  ;(println "mode" mode)
  ({:normal-mode 0
    :insert-mode 1
    :ex-mode 2} mode))

(defn- buf-submode [mode]
  ;(println "submode" mode)
  ({:none 0
    :temp-normal-mode 1} mode))

(defn- update-visual [buf]
  (if (-> :visual buf nil? not)
    (update buf :visual (fn [visual]
                          (-> visual
                              (dissoc :range)
                              (assoc :type (visual-type (visual :type)))))) buf))

(defn- update-mode [buf]
  (let [buf (if (-> buf :mode nil? not)
              (update buf :mode buf-mode) buf)]
    (if (-> buf :submode nil? not)
      (update buf :submode buf-submode) buf)))

(defn- remove-fields [buf]
  (-> buf 
      (dissoc :expandtab :CRLF? :history :context :last-cursor 
              :language :filepath :x :y :cursor :keymap 
              :normal-mode-keymap :insert-mode-keymap :ex-mode-keymap
              :pending-undo :saved-undo :registers :linescnt :keys
              :save-point :ext :last-visual :nextid :dot-repeat-keys
              :last-indents :mod-time :autocompl-provider)
      (dissoc-empty [:changes])
      update-visual
      update-mode
      (dissoc-nil :keys)
      line-editor))

(defn- render 
  "Write changes to browser."
  [before after]
  (cond (= before after)
        nil
        (or (nil? before) (not (= (:id before) (:id after))))
        (-> after
            (assoc :str (-> after :str str))
            (assoc :lang (-> after :language :name))
            (dissoc :changes)
            (remove-autocompl before)
            remove-fields)
        :else
        (-> after
            (dissoc-if-equal before :id)
            (dissoc-if-equal before :dirty)
            (dissoc-if-equal before :line-buffer)
            (dissoc-if-equal before :autocompl)
            (remove-autocompl before)
            (dissoc-if-equal before :visual)
            (dissoc-if-equal before :scroll-top)
            (dissoc-if-equal before :mode)
            (dissoc-if-equal before :submode)
            (dissoc-if-equal before :brackets)
            (dissoc-if-equal before :keys)
            (dissoc-if-equal before :name)
            (dissoc-if-equal before :dirty)
            (dissoc-if-equal before :message)
            (dissoc-if-equal before :highlights)
            (dissoc-if-equal before :tabsize)
            (dissoc-if-equal before :pos)
            (dissoc-if-equal before :showkeys)
            (dissoc :str)
            remove-fields)))

(defn- ui-agent []
  (*window* :ui))

(listen
  :create-window
  (fn [window]
    (assoc window
           :ui
           (agent
             {:viewport {:w 0 :h 0} :render! (fn [a b] a)}
             :error-handler
             (fn [ui err]
               (println "ui agent fail:")
               (println ":bufid " (-> ui :buf :id))
               (println ":filepath " (-> ui :buf :filepath))
               (println err))))))

(defn viewport []
  (@(ui-agent) :viewport))

(defn update-ui
  ([f]
    (send (ui-agent) f))
  ([f a]
    (send (ui-agent) f a))
  ([f a b]
    (send (ui-agent) f a b)))

(defn get-from-ui [key]
  (if (nil? *window*)
    nil
    (@(ui-agent) key)))

(defn- bound-scroll-top
  "Change scroll top make cursor inside viewport"
  [buf]
  (let [st (buf :scroll-top)]
    (assoc buf :scroll-top
           (let [y (buf :y)
                 h ((viewport) :h)]
             (cond
               (zero? h) 0
               (< y st) y
               (< y (+ st h)) st
               (neg? (-> y (- h) inc)) 0
               :else (-> y (- h) inc))))))

(defn send-buf! [newbuf]
  (let [newbuf (bound-scroll-top newbuf)]
    (send-off (ui-agent) 
              (fn [{buf :buf :as ui} newbuf]
                (let [diff (render buf newbuf)
                      ui (assoc ui :buf (dissoc newbuf :changes))]
                  (if (empty? diff) ui
                      ((ui :render!) ui diff)))) newbuf)
    (assoc newbuf :changes [])))

(defn ui-buf []
  (render nil (get-from-ui :buf)))

(listen :change-buffer
        (fn [buf oldbuf c]
          ;changes of current command, only for writing back to client
          (update buf :changes conj c)))

