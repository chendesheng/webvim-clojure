(ns webvim.core.ui
  (:require [webvim.core.event :refer [listen]]
            [webvim.core.lineindex :refer [total-lines pos-xy]]
            [clojure.data :refer [diff]]
            [webvim.core.utils :refer [map-vals]])
  (:use webvim.core.line))

(defn- dissoc-empty [buf ks]
  (if (empty? (get-in buf ks))
    (if (= 1 (count ks))
      (dissoc buf (first ks))
      (recur (update-in buf (pop ks) dissoc (peek ks)) (pop ks)))
    buf))

(defn- dissoc-nil [buf k]
  (if (-> buf (get k) nil?)
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
  (if (-> buf :line-buffer nil?)
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
  (if (-> buf :visual some?)
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
              :save-point :ext :last-visual :dot-repeat-keys
              :last-indents :mod-time :autocompl-provider :lineindex
              :grammar :rule-stacks :scopes :str)
      (dissoc-empty [:changes])
      (dissoc-empty [:scope-changes])
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

(defn- diff-buf
  "Write changes to browser."
  [before after]
  (cond (= before after)
        nil
        (or (nil? before) (not (= (:id before) (:id after))))
        (-> after
            (assoc :lang (-> after :language :name))
            (assoc :changes [{:a [0 0] :b [0 0] :to (-> after :str str)}])
            (assoc :scope-changes [[0 (after :scopes)]])
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
            (dissoc-if-equal before :beep?)
            (dissoc-if-equal before :view)
            remove-fields)))

(defn- bound-scroll-top
  "Change scroll top make cursor inside viewport"
  [buf h]
  (update buf :scroll-top
          (fn [st]
            (let [y (buf :y)]
              ;(println "bound-scroll-top: h=" h "y=" y)
              (cond
                (nil? h) 0
                (zero? h) 0
                (< y st) y
                (< y (+ st h)) st
                (neg? (-> y (- h) inc)) 0
                :else (-> y (- h) inc))))))

(defn- visible-buffers [buffers]
  (into {}
        (filter (fn [[_ buf]]
                  (-> buf :view some?)) buffers)))

(defn- map-diff [m1 m2 fn-diff-val]
  (let [keys1 (keys m1)
        keys2 (keys m2)
        [only1 only2 both] (diff keys1 keys2)
        ;_ (println "(diff keys1 keys2) = " only1 only2 both)
        removed (partial reduce (fn [res key]
                                  (assoc res key nil)))
        added (partial reduce (fn [res key]
                                (assoc res key (fn-diff-val nil (m2 key)))))
        changed (partial reduce (fn [res key]
                                  (let [diff (fn-diff-val (m1 key) (m2 key))]
                                    (if (nil? diff)
                                      res
                                      (assoc res key diff)))))]
    (-> {}
        (removed only1)
        (added only2)
        (changed both))))

(defn- diff-buffers [buffers old-buffers]
  (if (not= buffers old-buffers)
    (map-diff (visible-buffers old-buffers)
              (visible-buffers buffers)
              diff-buf)))

(defn diff-window [window old-window]
  (-> (select-keys window [:id :active-buffer :views :cwd])
      (dissoc-if-equal old-window :id)
      (dissoc-if-equal old-window :active-buffer)
      (dissoc-if-equal old-window :views)
      (dissoc-if-equal old-window :cwd)
      (assoc :buffers (diff-buffers (:buffers window)
                                    (:buffers old-window)))
      (dissoc-nil :buffers)))

(defn- print-window [window]
  (update window :buffers
          (fn [buffers]
            (map-vals (fn [buf]
                        (-> buf
                            remove-fields
                            (dissoc :autocompl :str :highlights)
                            (dissoc :scopes)
                            (dissoc :scope-changes))) buffers))))

(listen :window-changed
        (fn [{render! :render! :as window} old-window _]
          ;(println "window:" (print-window window))
          ;(println "old-window:" (print-window old-window))
          ;(println "diff-window:" (print-window (diff-window window old-window)))
          (render! window (diff-window window old-window))
          (update window
                  :buffers
                  (fn [buffers]
                    (map-vals #(-> %
                                   (assoc :changes [])
                                   (dissoc :scope-changes)) buffers)))))

(listen :buffer-changed
        (fn [buf _ window]
          (-> buf
              (bound-scroll-top (-> window :viewport :h))
              (assoc :lines (-> buf :lineindex total-lines))
              pos2xy
              highlights2xy
              visual2xy)))

(listen :change-buffer
        (fn [buf oldbuf c]
          ;(println "change:" c)
          ;changes of current command, only for writing back to client
          (update buf :changes conj c)))

