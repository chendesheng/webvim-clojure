(ns webvim.core.ui
  (:use webvim.core.line
        webvim.core.buffer))

(defn- dissoc-empty[buf ks]
  (if (empty? (get-in buf ks))
    (if (= 1 (count ks))
      (dissoc buf (first ks))
      (recur (update-in buf (pop ks) dissoc (peek ks)) (pop ks)))
    buf))

(defn- dissoc-nil[buf k]
  (if (nil? (buf k))
    (dissoc buf k)
    buf))

(defn- dissoc-if-equal[after before k]
  (if (and (not (nil? before))
           (= (before k) (after k)))
    (dissoc after k)
    after))

(defn- remove-autocompl[{autocompl :autocompl :as after} before]
  (if (-> autocompl :suggestions count (<= 1))
    (assoc after :autocompl nil)
    (assoc after :autocompl
           (-> autocompl
               (dissoc-if-equal (:autocompl before) :suggestions)
               (dissoc :words)
               (dissoc :uncomplete-word)
               (dissoc :replace)
               (dissoc :limit-number)))))

(defn- line-editor[buf]
  (if (nil? (buf :line-buffer))
    buf
    (let [{s :str
           prefix :prefix
           pos :pos} (-> buf :line-buffer)] 
      (-> buf
          (update-in [:line-buffer] dissoc :prefix)
          (update-in [:line-buffer] assoc :str (str prefix s))
          (update-in [:line-buffer] assoc :pos (+ pos (count prefix)))))))

(defn- remove-visual[buf]
  (if (-> :visual buf nil? not)
    (update-in buf [:visual] dissoc :range) buf))

(defn- remove-fields[buf]
  (-> buf 
      (dissoc :expandtab :CRLF? :history :context :last-cursor 
              :language :filepath :x :y :cursor :keymap 
              :normal-mode-keymap :insert-mode-keymap :ex-mode-keymap
              :pending-undo :saved-undo :registers :linescnt 
              :save-point :ext :last-visual :nextid :dot-repeat-keys :last-indents)
      (dissoc-empty [:changes])
      remove-visual
      (dissoc-nil :keys)
      line-editor))

(defn- diff-dirty[after before]
  (let [a (dirty? after)
        b (dirty? before)]
    ;(println "diff-dirty:" a b)
    (if (= a b) after
      (assoc after :dirty a))))

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
                    (diff-dirty before)
                    (dissoc-if-equal before :line-buffer)
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
                    (dissoc :str)
                    remove-fields))]
    buf))

(defonce ui-agent (agent {:viewport {:w 0 :h 0}
                          :render! (fn[a b] a)}))

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

(defn send-buf![newbuf]
  (let [newbuf (bound-scroll-top newbuf)]
    (send-off ui-agent 
              (fn[{buf :buf :as ui} newbuf]
                (let [diff (render buf newbuf)
                      render! (ui :render!)]
                  (-> ui
                      (render! diff) ;I/O
                      (assoc :buf (-> newbuf 
                                      (dissoc :changes)))))) newbuf)
    (assoc newbuf :changes [])))

(defn ui-buf[]
  (render nil (@ui-agent :buf)))
