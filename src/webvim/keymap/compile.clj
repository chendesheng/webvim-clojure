(ns webvim.keymap.compile
  (:use webvim.core.utils
        webvim.core.event))

(defn- fire-before-handle-key[buf keycode]
  (fire-event :before-handle-key buf keycode)) 

(defn- record-macro[buf keycode]
  (update-in buf [:dot-repeat-keys] conj keycode))

(defn- save-key[buf keycode]
  (update-in buf [:keys] conj keycode))

(defn- stop[buf keycode]
  false)

(defn- keycode-func-comp[funcs]
  (let [funcs (filter #(and (not (nil? %)) (not (= % nop))) funcs)]
    ;(pprint funcs)
    (if (empty? funcs)
      nil
      (fn[buf keycode]
        (reduce
         (fn[buf f]
           (f buf keycode)) buf (reverse funcs))))))

(defn- leave-map[buf [_ {after :after continue? :continue leave :leave}]]
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

(def ^:private cache (atom {}))

(defn compile-keymap[keymap]
  (if (nil? (@cache keymap))
  (swap! cache assoc keymap (tree-reduce
    (fn[ctx [[_ {enter :enter leave :leave else :else}] [_ {before :before}] & _ :as path]]
      (let [leave (or leave nop)
            ctx1 (if (nil? else)
                   (assoc ctx (clojure.string/join (conj (map key path) ":else")) 
                     (fn[buf keycode]
                       (reduce leave-map
                               (leave buf keycode)
                               (pop path))))
                   ctx)]
        (assoc ctx1
          (clojure.string/join (map key path))
          (keycode-func-comp
            (list enter before fire-before-handle-key record-macro save-key)))))
    
    (fn[ctx [[keycode func] & [[_ {before :before after :after continue? :continue}] & _ :as allparents] :as path]]
      (if (contains? #{:enter :leave :before :after :continue} keycode)
        ctx
        (let [func (if (= keycode :else)
                     func
                     (fn[buf keycode]
                       (func buf)))
              funcs (list func before fire-before-handle-key record-macro save-key)]
          ;(println "keycode:" keycode)
          (assoc ctx
            (clojure.string/join (map key path))
            (keycode-func-comp 
              (conj funcs
                    (fn[buf keycode]
                      ;(println "keycode:" keycode)
                      (if (empty? allparents) buf
                        (reduce leave-map buf allparents)))))))))
    {}
    keymap)))
  (@cache keymap))


