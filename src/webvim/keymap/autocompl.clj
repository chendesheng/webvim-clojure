(ns webvim.keymap.autocompl
  (:require
    [webvim.keymap.compile :refer [wrap-key]])
  (:use clojure.pprint
        webvim.core.buffer
        webvim.core.event
        webvim.core.rope
        webvim.core.line
        webvim.core.ui
        webvim.core.pos
        webvim.core.lang
        webvim.core.utils
        webvim.fuzzy
        webvim.keymap.ex
        webvim.keymap.objects
        webvim.autocompl))

(defn- limit-suggestions [fn-suggest w words limit-number]
  (let [suggestions (fn-suggest w words)]
    (if (pos? limit-number)
      (vec (take limit-number suggestions))
      (vec suggestions))))

(defn- new-autocompl [buf
                      {:keys [uncomplete-word
                              limit-number
                              fn-words
                              fn-suggest
                              async]}]
  (println "new-autocompl")
  (if (-> buf :autocompl nil?) 
    (let [w (uncomplete-word buf)]
      (if (nil? w) buf
          (if async
            (let [all-words (partition-all limit-number (fn-words buf w))
                  autocompl (assoc buf :autocompl
                                   {:words nil
                                    :w w
                                    :suggestions nil
                                    :index 0
                                    :async-done? false})]
              (future
                (println "future")
                (let [abuf (get-buffer-agent (buf :id))
                      autocompl-w (fn [] (-> @abuf :autocompl :w))
                      autocompl-some? (fn [] (-> @abuf :autocompl some?))]
                  (loop [words (first all-words)
                         rest-words (next all-words)]
                    ;(println (empty? rest-words))
                    ;(pprint (empty? words))
                    ;(pprint (-> words first nil?))
                    ;(pprint (first words))
                    ;stop generate words when :autocompl is nil
                    (let [suggestions (limit-suggestions fn-suggest (autocompl-w) words limit-number)]
                      ;(pprint suggestions)
                      (send abuf
                            (fn [buf words suggestions]
                              (if (-> buf :autocompl nil?)
                                buf
                                (-> buf
                                    (update :autocompl
                                            assoc :words words :suggestions suggestions)
                                    send-buf!))) words suggestions))
                    (if (and (autocompl-some?) (some? rest-words))
                      (recur (concat words (first rest-words))
                             (next rest-words))
                      ;spin and re-calculate suggestions when :w changes until autocompl session ends by check (-> buf :autocompl nil?) is true
                      (loop [oldw (autocompl-w) 
                             w (autocompl-w)]
                        (if (not= oldw w)
                          (send abuf (fn [buf suggestions]
                                       (-> buf
                                           (assoc-in [:autocompl :suggestions] suggestions)
                                           send-buf!)) (limit-suggestions fn-suggest w words limit-number))
                          (Thread/sleep 10))
                        (if (autocompl-some?)
                          (recur w (autocompl-w))))))))
              autocompl)
            (assoc buf :autocompl
                   ;remove current word
                   ;words is fixed during auto complete
                   {:words (fn-words buf w)
                    :w w
                    :suggestions nil
                    :index 0}))))
    buf))

(defn- autocompl-suggest [{{:keys [words]} :autocompl :as buf}
                          {:keys [limit-number
                                  uncomplete-word
                                  fn-suggest
                                  async]}]
  (let [w (uncomplete-word buf)]
    (if (nil? w)
      (assoc buf :autocompl nil) ;stop if no uncomplete word
      (update buf :autocompl assoc 
              :index 0 :w w
              :suggestions (if async
                             (-> buf :autocompl :suggestions)
                             (limit-suggestions fn-suggest w words limit-number))))))

(defn- autocompl-move [buf
                       {replace-suggestion :replace-suggestion :as provider}
                       f]
  (let [buf (if (empty? (-> buf :autocompl :suggestions))
              (autocompl-suggest buf provider) buf)]
    (if (-> buf :autocompl nil?)
      buf
      (let [{suggestions :suggestions i :index} (buf :autocompl)
            cnt (count suggestions)
            i (if (>= i cnt) (dec cnt) i)
            w (suggestions i)]
        (if (zero? cnt)
          buf
          (let [newi (mod (+ (f i) cnt) cnt)
                neww (suggestions newi)]
            (-> buf 
                (assoc-in [:autocompl :index] newi)
                (replace-suggestion neww w))))))))

(defn- start-autocompl [buf keycode
                        {start-autocompl? :start-autocompl?
                         continue-autocompl? :continue-autocompl?
                         :as provider}]
  (if (-> buf :autocompl nil?)
    (if (start-autocompl? buf keycode)
      (new-autocompl buf provider)
      buf)
    (if (continue-autocompl? buf keycode)
      buf
      (recur (assoc buf :autocompl nil) keycode provider))))

(defn- extends-autocompl [keymap provider]
  (-> keymap
      (wrap-key :leave
                (fn [handler]
                  (fn [buf keycode]
                    (-> buf
                        (assoc :autocompl nil)
                        (handler keycode)))))
      ;do autocompl after insert char to buffer
      (wrap-key :after
                (fn [handler]
                  (fn [buf keycode]
                    (println "after:")
                    (let [buf (start-autocompl buf keycode provider)]
                      (if (-> buf :autocompl nil?)
                        (handler buf keycode)
                        (let [buf (handler buf keycode)]
                          (cond
                            (= keycode (provider :move-up))
                            (autocompl-move buf provider dec)
                            (= keycode (provider :move-down))
                            (autocompl-move buf provider inc)
                            :else
                            (autocompl-suggest buf provider))))))))))

(defn- word-char-under-cursor? [buf]
  (let [{not-word-chars :not-word-chars} (word-re (buf :language))
        ch (str (.charAt (buf :str) (buf :pos)))]
    (not (re-test (re-pattern (str "[" not-word-chars "]")) ch))))

(def default-provider {:move-up "<c-p>"
                       :move-down "<c-n>"
                       :uncomplete-word buffer-uncomplete-word
                       :replace-suggestion (fn [buf item olditem]
                                             ;(println "replace")
                                             ;(pprint item)
                                             ;(pprint olditem)
                                             (buffer-replace-suggestion buf (item :name) (olditem :name)))
                       :async false
                       :fn-words (fn [buf w]
                                   (map (fn [w]
                                          {:name w})
                                        (keys
                                          (let [cw (str (current-word buf))]
                                            ;(println w cw)
                                            (if (or (not (word-char-under-cursor? buf))
                                                    (= w cw)
                                                    (empty? cw))
                                              (autocompl-remove-word @autocompl-words w)
                                              (-> @autocompl-words
                                                  (autocompl-remove-word w)
                                                  (autocompl-remove-word cw)
                                                  (autocompl-add-word (subs cw (count w)))))))))
                       :fn-suggest fuzzy-suggest
                       :limit-number 0
                       :start-autocompl? (fn [buf keycode]
                                           (or (= keycode "<c-p>")
                                               (= keycode "<c-n>")))
                       :continue-autocompl? (fn [_ keycode] 
                                              (not= keycode "<esc>"))})

(listen
  :new-buffer
  (fn [buf]
    (assoc buf :autocompl-provider default-provider)))

(listen
  :insert-mode-keymap
  (fn [keymap buf]
    (extends-autocompl keymap
                       (fire-event :new-autocompl-provider default-provider buf))))

(listen
  :temp-normal-mode-keymap
  (fn [keymap _]
    (wrap-key keymap :enter
              (fn [handler]
                (fn [buf keycode]
                  (-> buf
                      (assoc :autocompl nil)
                      (handler keycode)))))))

(listen
  :ex-mode-keymap
  (fn [keymap _]
    (extends-autocompl keymap ex-autocompl-provider)))
