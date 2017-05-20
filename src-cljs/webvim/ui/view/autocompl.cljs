(ns webvim.ui.view.autocompl
  (:require
    [clojure.string :as string]
    [webvim.fuzzy :refer [fuzzy-match]]
    [webvim.ui.lib.dom :refer [bounding-rect $id $text-content
                               toggle-class add-class $show $status-bar-buf
                               $hide $empty $hiccup rect-pos $view rect
                               get-element-and-offset line-height]]))

(defn- cursor-position [$lines-nodes px py]
  (let [$cur-line (aget $lines-nodes py)]
    (if (some? $cur-line)
      (let [rect (apply bounding-rect
                        (get-element-and-offset $cur-line px))
            [x y] (rect-pos rect)
            h (.-height rect)]
        ;(println "xy:" x y)
        [x (dec y) h]))))

(defn- unit-px [f]
  (str f "px"))

(defn render-autocompl-inner [$autocompl bufid view sugs i ex-autocompl? px py]
  (if (-> sugs count (> 1))
    (let [subject (-> sugs first :name)
          selected-sug ((nth sugs i) :name)]
      ($empty $autocompl)
      (doseq [{nm :name cls :class} (rest sugs)]
        (.appendChild $autocompl
                      (let [$item ($hiccup [:pre.with-class {:class cls}])
                            indexes (fuzzy-match nm subject)]
                        ;(println indexes)
                        (loop [a 0
                               indexes (seq indexes)]
                          (if indexes
                            (let [[b & indexes] indexes
                                  text (.substring nm a b)
                                  matched (.substr nm b 1)]
                              (when-not (empty? text)
                                (.appendChild $item (js/document.createTextNode text)))
                              (when-not (empty? matched)
                                (.appendChild $item ($hiccup [:span.matched matched])))
                              (recur (inc b) indexes))
                            (let [text (.substr nm a)]
                              (when-not (empty? text)
                                (.appendChild $item (js/document.createTextNode text)))
                              $item))))))
      (if (pos? i)
        (-> $autocompl .-childNodes (aget (dec i)) (add-class "highlight")))
      (toggle-class $autocompl "ex-autocompl" ex-autocompl?)
      (if ex-autocompl?
        (let [[x y x1 y1] (-> view $view bounding-rect rect)
              bottom (- (.-offsetHeight js/document.body) y1)]
          (doto (.-style $autocompl)
            (-> .-left (set! (unit-px x)))
            (-> .-bottom (set! (unit-px bottom)))
            (-> .-marginBottom (set! "1.8em"))
            (-> .-marginLeft (set! "3ch"))))
        (let [[x y1 h] (cursor-position (-> ($id (str "lines-" bufid)) .-childNodes) (- px (count selected-sug)) py)
              autocomplh (* (min (dec (count sugs)) 12) h)
              [top margin-top] (if (> (+ y1 h autocomplh 5) js/window.innerHeight)
                                 [(- y1 autocomplh) -5]
                                 [(+ y1 h) 5])]
          (doto (.-style $autocompl)
            (-> .-left (set! (str x "px")))
            (-> .-top (set! (str top "px")))
            (-> .-marginTop (set! (str margin-top "px")))
            (-> .-marginLeft (set! "-1ch")))))
      ($show $autocompl))
    ($hide $autocompl)))

(def ex-mode 2)

(defn render-autocompl [{old-autocompl :autocompl}
                        {{sugs :suggestions
                          i :index :as autocompl} :autocompl
                         bufid :id
                         view :view
                         [px py] :cursor
                         mode :mode}]
  (let [$autocompl ($id "autocompl")]
    (cond
      (not= old-autocompl autocompl)
      (render-autocompl-inner $autocompl bufid view sugs i (= mode ex-mode) px py)
      (or (nil? autocompl) (empty? sugs))
      ($hide $autocompl))))
