(ns webvim.core.ui
  (:use webvim.core.line))

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

(def ^:private visual-line 1)

(defn- remove-visual-mode[buf]
  (if (empty? (-> buf :visual :ranges))
    (dissoc buf :visual)
    (if (= (-> buf :visual :type) visual-line)
      (update-in buf [:visual :ranges] 
                 (fn[ranges]
                   (map #(make-linewise-range % buf) ranges)))
      buf)))

(defn- remove-autocompl[buf]
  (if (empty? (-> buf :autocompl :suggestions))
    (dissoc buf :autocompl)
    buf))

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

(defn- remove-fields[buf]
  (-> buf 
      (dissoc :history :context :last-cursor :language :filepath :x :y :cursor :pending-undo :macro :registers :linescnt :root-keymap :ext :last-visual :nextid)
      (dissoc-empty [:changes])
      (dissoc-nil :keys)
      line-editor
      remove-visual-mode
      remove-autocompl))

(defn- dissoc-if-equal[after before k]
  (if (= (before k) (after k))
    (dissoc after k)
    after))

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
                    remove-fields)
                :else
                (-> after
                    (dissoc-if-equal before :line-buffer)
                    remove-fields
                    (dissoc :str)
                    (dissoc-if-equal before :mode)
                    (dissoc-if-equal before :braces)
                    (dissoc-if-equal before :keys)
                    (dissoc-if-equal before :name)
                    (dissoc-if-equal before :dirty)
                    (dissoc-if-equal before :message)
                    (dissoc-if-equal before :highlights)
                    (dissoc-if-equal before :pos)))]
    buf))


(defonce ui-agent (agent {:viewport {:w 0 :h 0}
                      :render! (fn[a b])}))

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
              (fn[{buf :buf :as ui}]
                (let [diff (render buf newbuf)
                      render! (ui :render!)]
                  (-> ui
                      (render! diff) ;I/O
                      (assoc :buf (-> newbuf 
                                      (dissoc :changes)
                                      (dissoc :history)))))))
    (assoc newbuf :changes [])))

(defn ui-buf[]
  (render nil (@ui-agent :buf)))
