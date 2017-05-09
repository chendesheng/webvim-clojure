(ns webvim.core.ui
  (:require [webvim.core.event :refer [listen]]
            [webvim.core.lineindex :refer [total-lines pos-xy]]
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

(defn- dissoc-false [buf k]
  (if (buf k)
    buf (dissoc buf k)))

(defn- dissoc-if-equal [after before k]
  (if (and (some? before)
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
  (if (-> :visual buf some?)
    (update buf :visual (fn [visual]
                          (-> visual
                              (dissoc :range)
                              (assoc :type (visual-type (visual :type)))))) buf))

(defn- update-mode [buf]
  (let [buf (if (-> buf :mode some?)
              (update buf :mode buf-mode) buf)]
    (if (-> buf :submode some?)
      (update buf :submode buf-submode) buf)))

(defn- remove-fields [buf]
  (-> buf
      (dissoc :expandtab :CRLF? :history :context :last-cursor
              :language :filepath :x :y :keymap
              :normal-mode-keymap :insert-mode-keymap :ex-mode-keymap
              :pending-undo :saved-undo :registers :keys
              :save-point :ext :last-visual :nextid :dot-repeat-keys
              :last-indents :mod-time :autocompl-provider :lineindex)
      (dissoc-empty [:changes])
      update-visual
      update-mode
      (dissoc-nil :keys)
      line-editor))

(defn- pos2xy [buf]
  (if (-> buf :pos some?)
    (assoc buf :cursor (pos-xy (buf :lineindex) (buf :pos)))
    buf))

(defn- poses2xy [buf ranges]
  (let [pos-xy (partial pos-xy (buf :lineindex))]
    (map (fn [[a b]]
           [(pos-xy a) (pos-xy b)]) ranges)))

(defn- visual2xy [buf]
  (if (-> buf :visual some?)
    (update buf :visual (fn [{ranges :ranges :as visual}]
                          (println ranges)
                          (assoc visual :ranges2 (poses2xy buf ranges))))
    buf))

(defn- highlights2xy [buf]
  (if (-> buf :highlights some?)
    (assoc buf :highlights2
           (poses2xy buf (buf :highlights)))
    buf))

(defn- ui-agent []
  (*window* :ui))

(defn- window-data [window]
  {:id (window :id)
   :cwd @(window :cwd)})

(defn- diff-window [before after]
  (if (= before after) nil
      (-> after
          (dissoc-if-equal before :id)
          (dissoc-if-equal before :cwd))))

(defn- diff-buf
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
            (dissoc-if-equal before :highlights2)
            (dissoc-if-equal before :tabsize)
            (dissoc-if-equal before :cursor)
            (dissoc-if-equal before :cursor2)
            (dissoc-if-equal before :pos)
            (dissoc-if-equal before :showkeys)
            (dissoc-if-equal before :lines)
            (dissoc-if-equal before :scope-changes)
            (dissoc-if-equal before :beep?)
            (dissoc :str)
            remove-fields)))

;FIXME: doesn't feel right to put window object inside buf
(defn- diff-ui [{buf :buf window :window :as ui} newbuf]
  (let [diff (diff-buf buf newbuf)
        diffwin (diff-window window (window-data *window*))]
    (if (nil? diffwin) diff ;write array back if window object changes
        [diffwin diff])))

(listen
  :create-window
  (fn [window]
    (assoc window
           :ui
           (agent
             {:viewport {:w 0 :h 0}
              :render! (fn [a b] a)
              :window (window-data window)}
             :error-handler
             (fn [ui err]
               (println "ui agent fail:")
               (println ":bufid " (-> ui :buf :id))
               (println ":filepath " (-> ui :buf :filepath))
               (println err))))))

(defn update-ui
  ([f]
    (send (ui-agent) f))
  ([f a]
    (send (ui-agent) f a))
  ([f a b]
    (send (ui-agent) f a b)))

(defn get-from-ui [key]
  (@(ui-agent) key))

(defn viewport []
  (get-from-ui :viewport))

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

(defn- active-buf? [ui buf]
  (= (buf :id) (-> ui :buf :id)))

(defn send-buf!
  ([buf switch-buf?]
    (let [buf (bound-scroll-top buf)]
      (send-off (ui-agent)
                (fn [ui buf]
                  (if (or switch-buf? (active-buf? ui buf))
                    (let [buf (-> buf
                                  (assoc :lines (-> buf :lineindex total-lines))
                                  pos2xy
                                  highlights2xy
                                  visual2xy)
                          diff (diff-ui ui buf)
                          ui (assoc ui :buf (dissoc buf :changes))]
                      (println "cursor:" (buf :cursor))
                      (if (or (nil? diff)
                              (empty? diff)) ui
                          ((ui :render!) ui diff)))
                    ui))
                buf)
      (-> buf
          (assoc :changes [])
          (assoc :beep? false))))
  ([buf]
    ;only for active buffer, use :nextid for switch buffer
    (send-buf! buf false)))

(defn ui-buf []
  [(diff-window nil (get-from-ui :window))
   (diff-buf nil (get-from-ui :buf))])

(listen :change-buffer
        (fn [buf oldbuf c]
          (println "change:" c)
          ;changes of current command, only for writing back to client
          (update buf :changes conj c)))

