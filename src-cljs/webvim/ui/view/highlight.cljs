(ns webvim.ui.view.highlight
  (:require
    [clojure.string :as string]
    [webvim.ui.lib.dom :refer [bounding-rect $id $text-content toggle-class $hiccup]]))

(defn render-highlight-inner [$lines $highlights rg]
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
                              {:style (str "left:" x "px;" "top:" y "px;"
                                           "width:" w "px;" "height:" (line-height) "px;"
                                           "padding-right:1ch;")}])))))

