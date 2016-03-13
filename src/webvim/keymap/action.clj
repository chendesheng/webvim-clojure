;common actions
(ns webvim.keymap.action
  (:require [me.raynes.fs :as fs]
            [clojure.string :as string]
            [webvim.mode :refer [set-insert-mode set-normal-mode]]
            [webvim.visual :refer [visual-range visual-line visual-block]]
            [webvim.scrolling :refer [cursor-center-viewport]]
            [webvim.keymap.compile :refer [compile-keymap]])
  (:use webvim.core.buffer
        webvim.core.rope
        webvim.core.line
        webvim.core.pos
        webvim.core.register
        webvim.core.event
        webvim.fuzzy
        webvim.core.lang
        webvim.core.utils
        webvim.jumplist
        webvim.core.ui
        clojure.pprint))

(defn update-x [buf]
  (let [pos (buf :pos)
        r (buf :str)]
    (assoc buf :x (dec (visual-size 
                         (subr r (pos-line-first r pos) (inc pos)) 
                         (buf :tabsize))))))

(defn update-x-if-not-jk
  "update :x unless it is up down motion"
  [buf keycode]
  (let [lastbuf (buf :context :lastbuf)]
    (if-not (or (= (:pos lastbuf) (:pos buf))
                (contains? #{"j" "k" "<c-d>" "<c-u>"} keycode))
      (update-x buf) buf)))

(defn keycode-cancel [buf]
  (-> buf
      set-normal-mode
      (dissoc :context :keys :line-buffer)
      (assoc :visual {:type 0 :range [0 0]}
             :message ""
             :autocompl nil
             :showkeys nil)))

(defn- fire-before-handle-key [buf keycode]
  (fire-event :before-handle-key buf keycode)) 

(defn- fire-after-handle-key [buf keycode]
  (fire-event :after-handle-key buf keycode)) 

(defn apply-keycode [buf keycode]
  (if (= keycode "<c-c>")
    (keycode-cancel buf)
    (let [keymap (compile-keymap (buf :keymap))
          allkeycode (conj (buf :keys) keycode)
          func (or (keymap (clojure.string/join allkeycode))
                   (keymap (clojure.string/join (conj (buf :keys) ":else")))
                   (if (-> buf :keys empty? not)
                     (or
                       (keymap (clojure.string/join (conj (pop (buf :keys)) ":else" keycode))) ;last :else can be map too
                       (keymap (clojure.string/join (conj (pop (buf :keys)) ":else:else")))))
                   nop)]
      (-> buf
          (fire-before-handle-key keycode)
          (func keycode)
          (fire-after-handle-key keycode)))))

(defn apply-keycodes [buf keycodes]
  (reduce apply-keycode buf keycodes))

(defn normal-mode-fix-pos
  "prevent cursor on top of EOL in normal mode"
  [buf]
  (let [ch (char-at (buf :str) (buf :pos))]
    (if (= (or ch \newline) \newline)
      (char- buf) buf)))

(defn replay-keys [buf keycodes]
  (let [keys (buf :keys)] 
    (-> buf
        (dissoc :keys)
        (apply-keycodes keycodes)
        (assoc :keys keys))))

(defn special-key? [key]
  (contains? #{:enter :leave :before :after :else} key))

(defn wrap-key [keymap key f]
  (update keymap key (fn [handler]
                       (f (or handler nop)))))

(defn wrap-keycode [f]
  (fn [buf keycode]
    (f buf)))

(defn repeat-prefix-value [buf]
  (-> buf :context :repeat-prefix (or "1") parse-int))

(defn current-word [buf]
  (let [{pos :pos
         r :str
         lang :language} buf
        {word-chars :word-chars
         not-word-chars :not-word-chars} (word-re lang)
        re-start (re-pattern (str "([" not-word-chars "](?=[" word-chars "]))|((?<=[" not-word-chars "])$)"))
        re-end (re-pattern (str "[" word-chars "](?=[" not-word-chars "])"))
        b (or (last (pos-re+ r pos re-end)) (count r))
        a (or (last (pos-re- r (dec b) re-start)) 0)]
    (subr r a b)))

(defmacro async [buf & body]
  `(let [abuf# (@buffer-list (~buf :id))]
     (-> abuf#
         (send (fn [~'buf]
                 (let [buf# ~@body]
                   (send-buf! buf#)))))
     ~buf))

(defmacro with-catch [buf & body]
  `(try
     (do ~@body)
     (catch Exception e#
       (assoc ~buf :message (str e#)))))

(defmacro async-with-catch [buf & body]
  `(async ~buf
          (with-catch ~buf ~@body)))

(defn buf-match-bracket
  ([buf pos]
    (-> buf
        (assoc :brackets [])
        (async
          (let [mpos (pos-match-bracket (buf :str) pos)]
            (assoc buf :brackets 
                   (if (nil? mpos) [] [pos mpos]))))))
  ([buf]
    (buf-match-bracket buf (buf :pos))))
