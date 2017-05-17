(ns webvim.ui.view.visual
  (:require
    [clojure.string :as string]
    [webvim.ui.lib.dom :refer [bounding-rect $id $text-content
                               toggle-class $hiccup $empty
                               get-element-and-offset line-height
                               rect-pos rect-size]]))

(defn- lines-ranges [$lines [[ax ay] [bx by]]]
  (let [$lines-nodes (.-childNodes $lines)
        lines (reduce (fn [lines i]
                        (let [$line (aget $lines-nodes i)
                              length (-> $line .-textContent count)]
                          (conj lines [[0 i] [(dec length) i]]))) [] (range ay (inc by)))]
    (-> lines
        (assoc-in [0 0 0] ax)
        (assoc-in [(- by ay) 1 0] bx))))

(defn- get-rect [ele a b]
  (if (= a b)
    (let [rt (apply bounding-rect (get-element-and-offset ele a))]
      [(rect-pos rt) (rect-size rt)])
    (let [rt-left (apply bounding-rect (get-element-and-offset ele a))
          rt-right (apply bounding-rect (get-element-and-offset ele b))]
      [(rect-pos rt-left)
       [(- (.-right rt-right) (.-left rt-left))
        (.-height rt-left)]])))

(defn render-highlight [$lines $highlights rg]
  ;(println "render-highlight:" rg)
  ;(println rg)
  (doseq [[[ax ay] [bx by]] (lines-ranges $lines rg)]
    ;(println "range:" ax ay bx by)
    (let [$line (-> $lines .-childNodes (aget ay))
          [[x1 y1] [w h]] (get-rect $line ax bx)
          [x y] (let [[linesx linesy] (rect-pos (bounding-rect $lines))]
                  [(- x1 linesx) (- y1 linesy)])]
      (.appendChild $highlights
                    ($hiccup [:span.line-selected
                              {:style (str "left:" x "px;" "top:" (dec y) "px;"
                                           "width:" w "px;" "height:" (line-height) "px;"
                                           "padding-right:1ch;")}])))))

(defn render-highlights [{old-highlights :highlights2}
                         {highlights :highlights2
                          bufid :id}]
  (if (not= old-highlights highlights)
    (let [$highlights ($id (str "highlights-" bufid))]
      ($empty $highlights)
      (doseq [rg highlights]
        (render-highlight ($id (str "lines-" bufid)) $highlights rg)))))

(defn render-visual [{old-visual :visual}
                     {visual :visual
                      bufid :id}]
  (if (not= old-visual visual)
    (let [$selections ($id (str "selections-" bufid))]
      ($empty $selections)
      (doseq [rg (:ranges2 visual)]
        (render-highlight ($id (str "lines-" bufid)) $selections rg)))))

