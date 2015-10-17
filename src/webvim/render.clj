(ns webvim.render
  (:use webvim.core.buffer
        webvim.core.serve
        webvim.core.register
        webvim.core.keys
        webvim.keymap
        webvim.keymap.action))

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
      (dissoc :history :context :last-cursor :language :filepath :x :y :cursor :pending-undo :before-send-out :after-send-out :macro :chan-in :chan-out :registers :linescnt :root-keymap :ext :last-visual)
      (dissoc-empty [:highlights])
      (dissoc-empty [:changes])
      (dissoc-nil :keys)
      line-editor
      remove-visual-mode
      remove-autocompl))

(defn- dissoc-if-equal[after before k]
  (if (= (before k) (after k))
    (dissoc after k)
    after))

(defn render 
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
                    remove-fields
                    (dissoc :str)
                    (dissoc-if-equal before :mode)
                    (dissoc-if-equal before :keys)
                    (dissoc-if-equal before :name)
                    (dissoc-if-equal before :dirty)
                    (dissoc-if-equal before :message)
                    (dissoc-if-equal before :pos)))]
    buf))

