(ns webvim.ui.view.autocompl
  (:require
    [clojure.string :as string]
    [webvim.fuzzy :refer [fuzzy-match]]
    [webvim.ui.lib.dom :refer [bounding-rect $id $text-content
                               toggle-class add-class $show
                               $hide $empty $hiccup rect-pos
                               get-element-and-offset]]))

(defn- cursor-position [$lines-nodes px py]
  (let [$cur-line (aget $lines-nodes py)]
    (if (some? $cur-line)
      (let [rect (apply bounding-rect
                        (get-element-and-offset $cur-line px))
            [x y] (rect-pos rect)
            h (.-height rect)]
        ;(println "xy:" x y)
        [x (dec y) h]))))

(defn render-autocompl-inner [$autocompl bufid sugs i ex-autocompl? px py]
  (print "render-autocompl")
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
        (doto (.-style $autocompl)
          (-> .-left (set! ""))
          (-> .-top (set! ""))
          (-> .-marginTop (set! ""))
          (-> .-marginLeft (set! "")))
        (let [;rect (bounding-rect ($id (str "cursor-" bufid)))
                           ;x (.-left rect)
                           ;y (+ (.-top rect) (.-height rect))
              [x y1 h] (cursor-position (-> ($id (str "lines-" bufid)) .-childNodes) (- px (count selected-sug)) py)
              autocomplh (* (min (dec (count sugs)) 12) h)
              [top margin-top] (if (> (+ y1 h autocomplh 5) js/window.innerHeight)
                                 [(- y1 autocomplh) -5]
                                 [(+ y1 h) 5])]
          (doto (.-style $autocompl)
            (-> .-left (set! (str x "px")))
            (-> .-top (set! (str top "px")))
            (-> .-marginTop (set! (str margin-top "px")))
            (-> .-marginLeft (set! "-1ch")))))
      ($show $autocompl)
                   ;TODO: highlight matched characters
                   ;TODO: scroll to highlight item
      (comment let [lineh (-> $autocompl .-firstChild .-offsetHeight)
                    a (js/Math.floor ((.-scrollTop $autocompl) / lineh))
                    b (js/Math.floor ((+ (.-scrollTop $autocompl) (.-offsetHeight $autocompl)) / lineh))]
               (if-not (<= a i b))))
    ($hide $autocompl)))

(def ex-mode 2)

(defn render-autocompl [{old-autocompl :autocompl}
                        {{sugs :suggestions
                          i :index :as autocompl} :autocompl
                         bufid :id
                         [px py] :cursor
                         mode :mode}]
  (let [$autocompl ($id "autocompl")]
    (cond
      (not= old-autocompl autocompl)
      (render-autocompl-inner $autocompl bufid sugs i (= mode ex-mode) px py)
      (or (nil? autocompl)
          (empty? sugs))
      ($hide $autocompl))))
