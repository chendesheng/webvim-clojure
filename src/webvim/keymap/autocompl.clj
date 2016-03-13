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
        webvim.autocompl))

(defn- new-autocompl [buf
                      {uncomplete-word :uncomplete-word
                       limit-number :limit-number
                       fn-words :fn-words
                       fn-suggest :fn-suggest
                       async :async}]
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
                                    :index 0})]
              (future
                (println "future")
                (let [abuf (@buffer-list (buf :id))]
                  (loop [words (first all-words)
                         rest-words (next all-words)]
                  ;(println (empty? rest-words))
                  ;(pprint (empty? words))
                  ;(pprint (-> words first nil?))
                  ;(pprint (first words))
                  ;stop generate words when :autocompl is nil
                    (let [suggestions (vec (take limit-number
                                                 (fn-suggest (-> @abuf :autocompl :w) words)))]
                    ;(pprint suggestions)
                      (send abuf
                            (fn [buf words suggestions]
                              (if (-> buf :autocompl nil?)
                                buf
                                (-> buf
                                    (update-in [:autocompl]
                                               (fn [autocompl]
                                                 (assoc autocompl
                                                        :words words
                                                        :suggestions suggestions)))
                                    send-buf!))) words suggestions))
                    (if (and
                          (-> @abuf :autocompl nil? not)
                          (-> rest-words nil? not))
                      (recur (concat words (first rest-words))
                             (next rest-words))))))
              autocompl)
            (assoc buf :autocompl
                 ;remove current word
                 ;words is fixed during auto complete
                   {:words (fn-words buf w)
                    :w w
                    :suggestions nil
                    :index 0}))))
    buf))

(defn- autocompl-suggest [{{words :words :as autocompl} :autocompl :as buf}
                          {limit-number :limit-number
                           uncomplete-word :uncomplete-word
                           fn-suggest :fn-suggest}]
  (let [w (uncomplete-word buf)]
    (if (nil? w)
      (assoc buf :autocompl nil) ;stop if no uncomplete word
      (let [suggestions (fn-suggest w words)]
        ;(println "suggestions" suggestions)
        (-> buf 
            (assoc-in [:autocompl :index] 0)
            (assoc-in [:autocompl :w] w)
            (assoc-in [:autocompl :suggestions]
                      (if (pos? limit-number)
                        (vec (take limit-number suggestions))
                        (vec suggestions))))))))

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

(def default-provider {:move-up "<c-p>"
                       :move-down "<c-n>"
                       :uncomplete-word buffer-uncomplete-word
                       :replace-suggestion (fn [buf item olditem]
                                             ;(println "replace")
                                             ;(pprint item)
                                             ;(pprint olditem)
                                             (buffer-replace-suggestion buf (item :name) (olditem :name)))
                       :async false
                       :fn-words (fn [_ w]
                                   (map (fn [w]
                                          {:name w}) (keys (autocompl-remove-word @autocompl-words w))))
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
