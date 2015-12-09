(ns webvim.keymap.compile
  (:use webvim.keymap.action
        webvim.core.utils))

(defn record-keys[buf keycode]
  (if (nil? (#{"c+n" "c+p" "c+a" "c+x"} keycode)) ;Don't record keycode for these commands
    (update-in buf [:macro :recording-keys] conj keycode)
    buf))

(defn- stop[buf keycode]
  false)

(defn- keycode-func-comp[funcs]
  (let [funcs (filter (comp not nil?) funcs)]
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

(defn compile-keymap[keymap]
  (tree-reduce
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
            `(~bound-scroll-top ~#(update-in %1 [:keys] conj %2) ~enter ~before ~record-keys)))))
    
    (fn[ctx [[keycode func] & [[_ {before :before after :after continue? :continue}] & _ :as allparents] :as path]]
      (if (contains? #{:enter :leave :before :after :continue} keycode)
        ctx
        (let [func (if (= keycode :else)
                     func
                     (fn[buf keycode]
                       (func buf)))
              funcs `(~func ~before ~record-keys)]
              ;(println "keycode:" keycode)
          (assoc ctx
            (clojure.string/join (map key path))
            (keycode-func-comp 
              (conj funcs
                    (fn[buf keycode]
                    ;(println "keycode:" keycode)
                      (if (empty? allparents) buf
                        (let [buf (update-in buf [:keys] conj keycode)]
                        ;(println "keys:" (buf :keys))
                          (reduce leave-map buf allparents))))
                    bound-scroll-top))))))
    {}
    keymap))


