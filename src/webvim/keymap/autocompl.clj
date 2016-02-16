(ns webvim.keymap.autocompl
  (:use clojure.pprint
        webvim.keymap.action
        webvim.core.buffer
        webvim.core.event
        webvim.core.rope
        webvim.core.line
        webvim.core.pos
        webvim.core.lang
        webvim.core.utils
        webvim.fuzzy
        webvim.keymap.ex
        webvim.autocompl))

(defn- fuzzy-suggest [w words]
  (if (empty? w) nil
      (reduce #(conj %1 (last %2)) []
              (sort-by (juxt first second str)
                       (reduce 
                         (fn [suggestions word]
                           (let [indexes (fuzzy-match word w)]
                             (if (empty? indexes)
                               suggestions
                               (conj suggestions [(- (last indexes) 
                                                     (first indexes)) 
                                                  (first indexes) word])))) 
                         [[0 0 w]] words)))))

(defn- new-autocompl [buf
                      {uncomplete-word :uncomplete-word
                       limit-number :limit-number
                       fn-words :fn-words
                       async :async}]
  (println "new-autocompl")
  (if (-> buf :autocompl nil?) 
    (let [w (uncomplete-word buf)]
      (if (nil? w) buf
        (if async
          (let[all-words (partition-all 200 (fn-words w))
               autocompl (assoc buf :autocompl
                                {:words nil
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
                  (if (and (-> @abuf :autocompl nil? not))
                    (do
                      (let [suggestions (vec (take limit-number
                                                   (fuzzy-suggest (uncomplete-word @abuf) words)))]
                        ;(pprint suggestions)
                        (send abuf
                              (fn[buf words suggestions]
                                (if (-> buf :autocompl nil?)
                                  buf
                                  (-> buf
                                      (update-in [:autocompl]
                                                 (fn[autocompl]
                                                   (assoc autocompl
                                                          :words words
                                                          :suggestions suggestions)))
                                      webvim.core.ui/send-buf!))) words suggestions))
                      (if (-> rest-words nil? not)
                        (recur (concat words (first rest-words))
                               (next rest-words))))))))
            autocompl)
          (assoc buf :autocompl
                 ;remove current word
                 ;words is fixed during auto complete
                 {:words (fn-words w)
                  :suggestions nil
                  :index 0}))))
    buf))

(defn- autocompl-suggest [{{words :words :as autocompl} :autocompl :as buf}
                          {limit-number :limit-number
                           uncomplete-word :uncomplete-word}]
  (let [w (uncomplete-word buf)]
    (if (empty? w)
      (assoc buf :autocompl nil) ;stop if no uncomplete word
      (let [suggestions (fuzzy-suggest w words)]
        ;(println "suggestions" suggestions)
        (-> buf 
            (assoc-in [:autocompl :index] 0)
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
        (if (or (zero? cnt) (empty? w))
          buf
          (let [newi (mod (+ (f i) cnt) cnt)
                neww (suggestions newi)]
            (-> buf 
                (assoc-in [:autocompl :index] newi)
                (replace-suggestion neww w))))))))


(defn- extends-autocompl [keymap provider]
  (-> keymap
      (wrap-key (provider :move-up)
                (fn [handler]
                  (fn [buf keycode]
                    (-> buf
                        (handler keycode)
                        (new-autocompl provider)
                        (autocompl-move provider dec)))))
      (wrap-key (provider :move-down) 
                (fn [handler]
                  (fn [buf keycode]
                    (println "move-down")
                    (-> buf
                        (handler keycode)
                        (new-autocompl provider)
                        (autocompl-move provider inc)))))
      (wrap-key :leave
                (fn [handler]
                  (fn [buf keycode]
                    (-> buf
                        (assoc :autocompl nil)
                        (handler keycode)))))
      (wrap-key :after
                (fn [handler]
                  (fn [buf keycode]
                    ;continue checking until there is no suggestions
                    (if (or (-> buf :autocompl nil?)
                            (= keycode (provider :move-up))
                            (= keycode (provider :move-down)))
                      (handler buf keycode)
                      (-> buf
                          (handler keycode)
                          (autocompl-suggest provider))))))))

(defn- on-insert-mode-keymap [keymap]
  (extends-autocompl keymap
                     {:move-up "<c-p>"
                      :move-down "<c-n>"
                      :uncomplete-word buffer-uncomplete-word
                      :replace-suggestion buffer-replace-suggestion
                      :async false
                      :fn-words (fn [w] (keys (autocompl-remove-word @autocompl-words w)))
                      :limit-number 0}))

(defn- on-ex-mode-keymap [keymap]
  (extends-autocompl keymap ex-autocompl-provider))

(defn- on-temp-normal-mode-keymap [keymap]
  (wrap-key keymap :enter
            (fn [handler]
              (fn [buf keycode]
                (-> buf
                    (assoc :autocompl nil)
                    (handler keycode))))))

(defonce ^:private listener1
  (listen
    :insert-mode-keymap
    (fn [keymap]
      (on-insert-mode-keymap keymap))))

(defonce ^:private listener2
  (listen
    :temp-normal-mode-keymap
    (fn [keymap]
      (on-temp-normal-mode-keymap keymap))))

(defonce ^:private listener3
  (listen
    :ex-mode-keymap
    (fn [keymap]
      (on-ex-mode-keymap keymap))))
