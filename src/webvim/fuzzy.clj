(ns webvim.fuzzy)

(defn fuzzy-match 
  "return char indexs if word \"contains\" subject. length of subject must longer than 2"
  ([word subject case-insensitive?]
    (let [word (if case-insensitive?
                 (clojure.string/lower-case word)
                 word)
          subject (if case-insensitive?
                    (clojure.string/lower-case subject)
                    subject)
          indexes (reduce (fn [indexes ch]
                            (let [i (.indexOf word (int ch) 
                                              (if (empty? indexes)
                                                0
                                                (-> indexes last inc)))]
                              (if (neg? i)
                                (reduced [])
                                (conj indexes i)))) [] subject)]
      (if (empty? indexes)
        []
        (let [rindexes (loop [i (-> indexes last dec)
                              j (-> subject count dec dec)
                              rindexes (list (last indexes))]
                         (cond 
                           (neg? i)
                           []
                           (neg? j)
                           rindexes
                           :else
                           (if (= (.charAt word i) (.charAt subject j)) 
                             (recur (dec i) (dec j) (conj rindexes i))
                             (recur (dec i) j rindexes))))]
          (if (= (count indexes) (count rindexes))
            rindexes
            indexes)))))
  ([word subject]
    (fuzzy-match word subject false)))

(defn fuzzy-suggest [w words]
  (if (empty? w) nil
      (cons w
            (reduce #(conj %1 (last %2)) []
                    (sort-by (juxt first second str)
                             (reduce 
                               (fn [suggestions word]
                                 (let [indexes (fuzzy-match word w true)]
                                   (if (empty? indexes)
                                     suggestions
                                     (conj suggestions [(- (last indexes) 
                                                           (first indexes)) 
                                                        (first indexes) word])))) 
                               [] words))))))

