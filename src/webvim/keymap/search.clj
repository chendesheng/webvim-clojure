(ns webvim.keymap.search
  (:use webvim.core.pos
        webvim.core.buffer
        webvim.core.register
        webvim.core.lang
        webvim.core.rope
        webvim.core.line
        webvim.core.event
        webvim.core.ui
        webvim.core.parallel-universe
        webvim.keymap.linebuf.linebuf
        webvim.keymap.compile
        webvim.keymap.ex
        webvim.jumplist
        webvim.core.utils))

(defn- add-highlight [buf rg]
  (let [highlights (buf :highlights)]
    (if (empty? (filter (fn [[a b]]
                          (and (= a (rg 0)) (= b (rg 1)))) highlights))
      (update buf :highlights conj rg) buf)))

(defn re-forward-highlight [buf re]
  (let [pos (buf :pos)
        r (buf :str)
        rg (or
             (pos-re+ r (inc pos) re)
             (pos-re+ r 0 re))] ;TODO: use region reduce duplicate work
    (if (nil? rg) buf
        (let [[a b] rg]
          (-> buf
              (buf-set-pos a)
              (add-highlight [a (dec b)]))))))

(defn re-backward-highlight [buf re]
  (let [pos (buf :pos)
        r (buf :str)
        rg (or
             (pos-re- r (dec pos) re)
             (pos-re- r (-> r count dec) re))]
    (if (nil? rg) buf
        (let [[a b] rg]
          (-> buf
              (buf-set-pos a)
              (add-highlight [a (dec b)]))))))

(defn highlight-all-matches [buf re]
  ;(println "highlight-all-matches:" (buf :message))
  (let [r (buf :str)]
    (assoc buf :highlights 
           (map (fn [[a b]]
                  [a (dec b)])
                (pos-re-seq+ r 0 re)))))

(defn search-message [buf s forward?]
  (assoc buf :message (str (if forward? "/" "?") s)))

(defn search-pattern [s]
  (try
    (if (empty? s) 
      ;http://stackoverflow.com/questions/1723182/a-regex-that-will-never-be-matched-by-anything
      (re-pattern "(?m)(?!x)x")
      (if (re-test #"^[a-z\s\\{}()\[\]]*$" s)
        (re-pattern (str "(?m)(?i)" s))
        (re-pattern (str "(?m)" s))))
    (catch Exception e 
      (re-pattern "(?m)(?!x)x"))))

(defn- increment-search-<esc> [buf keycode]
  (-> buf
      (assoc :message "")
      (assoc :highlights [])
      (buf-set-pos (-> buf :context :lastpos))))

(defn- search-str [linebuf]
  (let [s (-> linebuf :str str)]
    (if (.startsWith s (linebuf :prefix))
      (:str (registers-get "/"))
      s)))

(defn- increment-search-<cr> [buf keycode]
  ;(println "increment-search-<cr>" (buf :message))
  (let [linebuf (buf :line-buffer)
        s (search-str linebuf)
        prefix (linebuf :prefix)]
    (registers-put! "/" {:str s :forward? (= prefix "/")})
    (-> buf
        (assoc :message (str prefix s))
        (highlight-all-matches (search-pattern s)))))

(defn- increment-search-after [forward?]
  (fn [buf keycode]
    (if (contains? #{"<cr>" "<esc>"} keycode)
      buf
      ;(println "increment-search" (buf :message))
      (let [linebuf (buf :line-buffer)]
        (if (nil? linebuf) buf
            (let [re (-> linebuf
                         search-str
                         search-pattern)
                  f (if forward?
                      re-forward-highlight 
                      re-backward-highlight)]
            ;(println "lastpos" (-> buf :context :lastpos))
              (-> buf
                  (buf-set-pos (-> buf :context :lastpos))
                  (assoc :highlights [])
                  (f re))))))))

(def ^:private linebuf-keymap (init-linebuf-keymap))

(defn increment-search-keymap [forward?]
  (-> linebuf-keymap
      (wrap-key :enter
                (fn [handler]
                  (fn [buf keycode]
                    (-> buf
                        (assoc-in [:context :lastpos] (buf :pos))
                        (handler keycode)))))
      (wrap-key :leave
                (fn [handler]
                  (fn [buf keycode]
                    (-> buf
                        (handler keycode)
                        (update :context dissoc :lastpos)))))
      (wrap-key :after
                (fn [handler]
                  (increment-search-after forward?)))
      (assoc "<esc>" increment-search-<esc> 
             "<cr>" increment-search-<cr>)))

