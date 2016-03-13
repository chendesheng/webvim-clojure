(ns webvim.keymap.compile
  (:require
    [webvim.mode :refer [set-normal-mode]]
    [webvim.core.utils :refer [nop tree-reduce]]
    [webvim.core.event :refer [fire-event]]))

(defn- save-key [buf keycode]
  (update-in buf [:keys] conj keycode))

(defn- stop [buf keycode]
  false)

(defn- keycode-func-comp [funcs]
  (let [funcs (filter #(and (not (nil? %)) (not (= % nop))) funcs)]
    ;(pprint funcs)
    (if (empty? funcs)
      nil
      (fn [buf keycode]
        (reduce
          (fn [buf f]
            (f buf keycode)) buf (reverse funcs))))))

(defn- leave-map [buf [_ {after :after continue? :continue leave :leave}]]
  (let [keycode (-> buf :keys first)
        after (or after nop)
        continue? (or continue? stop)
        leave (or leave nop)
        ;_ (println "keys:3" (buf :keys))
        buf1 (after buf keycode)]
        ;(println "keys:2" (buf1 :keys))
    (if (continue? buf1 keycode)
      (-> buf1
          (update-in [:keys] pop)
          reduced)
      (-> buf1
          (update-in [:keys] pop)
          (leave keycode)))))

(def compile-keymap
  (memoize
    (fn [keymap]
      (tree-reduce
        (fn [ctx [[_ {enter :enter leave :leave else :else}] [_ {before :before}] & _ :as path]]
          (let [leave (or leave nop)
                ctx1 (if (nil? else)
                       (assoc ctx (clojure.string/join (conj (map key path) ":else")) 
                              (fn [buf keycode]
                                (reduce leave-map
                                        (leave buf keycode)
                                        (pop path))))
                       ctx)]
            (assoc ctx1
                   (clojure.string/join (map key path))
                   (keycode-func-comp
                     (list enter before save-key)))))

        (fn [ctx [[keycode func] & [[_ {before :before after :after continue? :continue}] & _ :as allparents] :as path]]
          (if (contains? #{:enter :leave :before :after :continue} keycode)
            ctx
            (let [funcs (list func before save-key)]
              ;(println "keycode:" keycode)
              (assoc ctx
                     (clojure.string/join (map key path))
                     (keycode-func-comp 
                       (conj funcs
                             (fn [buf keycode]
                               ;(println "keycode:" keycode)
                               (if (empty? allparents) buf
                                   (reduce leave-map buf allparents)))))))))
        {}
        keymap))))

(defn- fire-before-handle-key [buf keycode]
  (fire-event :before-handle-key buf keycode)) 

(defn- fire-after-handle-key [buf keycode]
  (fire-event :after-handle-key buf keycode)) 

(defn keycode-cancel [buf]
  (-> buf
      set-normal-mode
      (dissoc :context :keys :line-buffer)
      (assoc :visual {:type 0 :range [0 0]}
             :message ""
             :autocompl nil
             :showkeys nil)))

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

