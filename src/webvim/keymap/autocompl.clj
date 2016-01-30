(ns webvim.keymap.autocompl
  (:use webvim.keymap.action
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

;TODO: autocompl provider interface

(defn- ex-replace-suggestion[buf w _]
  (let [news (str "e " w)]
        (update-in buf [:line-buffer]
                   (fn[linebuf]
                     (merge linebuf {:str (rope news)
                                     :pos (count news)})))))

(defn- ex-uncomplete-word[{{r :str} :line-buffer :as buf}]
  (let [[[_ w]] (re-seq #"^e\s(.*)" (-> buf :line-buffer :str str))] w))

(defmulti uncomplete-word (fn[buf] (buf :mode)))
(defmethod uncomplete-word insert-mode [buf]
  (buffer-uncomplete-word buf))
(defmethod uncomplete-word ex-mode [buf]
  (ex-uncomplete-word buf))

(defmulti replace-suggestion (fn[buf word oldword] (buf :mode)))
(defmethod replace-suggestion insert-mode [buf word oldword]
  (buffer-replace-suggestion buf word oldword))
(defmethod replace-suggestion ex-mode [buf word oldword]
  (ex-replace-suggestion buf word oldword))

(defn new-autocompl[buf fn-words]
  (println "new-autocompl insert-mode")
  (if (-> buf :autocompl nil?) 
    (let [w (uncomplete-word buf)]
      (if (nil? w) buf
        (assoc buf :autocompl
               ;remove current word
               ;words is fixed during auto complete
               {:words (fn-words w)
                :suggestions nil
                :index 0
                :limit-number 20})))
    buf))

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

(defn- autocompl-suggest[{{words :words
                          limit-number :limit-number :as autocompl} :autocompl :as buf}]
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

(defn- autocompl-move[buf f]
  (let [buf (if (empty? (-> buf :autocompl :suggestions))
              (autocompl-suggest buf) buf)]
    (if (-> buf :autocompl nil?)
      buf
      (let [{suggestions :suggestions i :index} (buf :autocompl)
            w (suggestions i)
            cnt (count suggestions)]
        (if (or (zero? cnt) (empty? w))
          buf
          (let [newi (mod (+ (f i) cnt) cnt)
                neww (suggestions newi)]
            (-> buf 
                (assoc-in [:autocompl :index] newi)
                (replace-suggestion neww w))))))))

(defn- buffer-words[w]
  (keys (autocompl-remove-word @autocompl-words w)))

(defn- filenames[w]
  (get-files))

(defn- extends-autocompl[keymap move-up move-down fn-words]
  (-> keymap
      (key-do-after move-up
                    (fn[buf]
                      (-> buf
                          (new-autocompl fn-words)
                          (autocompl-move dec))))
      (key-do-after move-down 
                    (fn[buf]
                      (println "move-down")
                      (-> buf
                          (new-autocompl fn-words)
                          (autocompl-move inc))))
      (key-do-before :leave
                     (fn[buf keycode]
                       (assoc buf :autocompl nil)))
      (key-do-after :else
                    (fn[buf keycode]
                        ;continue checking until there is no suggestions
                      (if (-> buf :autocompl nil?)
                        buf
                        (autocompl-suggest buf))))))

(defn- on-temp-normal-mode-keymap[keymap]
  (key-do-before keymap :enter
                 (fn[buf keycode]
                   (assoc buf :autocompl nil))))

(defonce ^:private listener1
  (listen
    :insert-mode-keymap
    (fn[keymap]
      (extends-autocompl keymap "<c-p>" "<c-n>" buffer-words))))

(defonce ^:private listener2
  (listen
    :temp-normal-mode-keymap
    (fn[keymap]
      (on-temp-normal-mode-keymap keymap))))

(defonce ^:private listener3
  (listen
    :ex-mode-keymap
    (fn[keymap]
      (extends-autocompl keymap "<s-tab>" "<tab>" filenames))))
