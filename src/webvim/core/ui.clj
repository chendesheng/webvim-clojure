(ns webvim.core.ui
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
           pos :pos} (-> buf :line-buffer)] 
      (update buf :line-buffer (fn [line-buf]
                                 (-> line-buf
                                     (dissoc :prefix)
                                     (assoc :str (str prefix s)
                                            :pos (+ pos (count prefix)))))))))

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
  (let [txt (after :str)
        buf (cond (= before after)
                  ""
                  (or (nil? before) (not (= (:id before) (:id after))))
                  (-> after
                      (assoc :str (str txt))
                      (assoc :lang (-> after :language :name))
                      (dissoc :changes)
                      (remove-autocompl before)
                      remove-fields)
                  :else
                  (-> after
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
                      remove-fields))]
    buf))

(defonce ui-agent (agent {:viewport {:w 0 :h 0}
                          :render! (fn [a b] a)} :error-handler (fn [ui err]
                                                                  (println "ui agent fail:")
                                                                  (println ":bufid " (-> ui :buf :id))
                                                                  (println ":filepath " (-> ui :buf :filepath))
                                                                  (println err))))

(defn- bound-scroll-top
  "Change scroll top make cursor inside viewport"
  [buf]
  (let [st (buf :scroll-top)]
    (assoc buf :scroll-top
           (let [y (buf :y)
                 h (-> @ui-agent :viewport :h)]
             (cond
               (zero? h) 0
               (< y st) y
               (< y (+ st h)) st
               (neg? (-> y (- h) inc)) 0
               :else (-> y (- h) inc))))))

(defn send-buf! [newbuf]
  (let [newbuf (bound-scroll-top newbuf)]
    (send-off ui-agent 
              (fn [{buf :buf :as ui} newbuf]
                (let [diff (render buf newbuf)
                      render! (ui :render!)]
                  (-> ui
                      (render! diff) ;I/O
                      (assoc :buf (-> newbuf 
                                      (dissoc :changes)))))) newbuf)
    (assoc newbuf :changes [])))

(defn ui-buf []
  (render nil (@ui-agent :buf)))
