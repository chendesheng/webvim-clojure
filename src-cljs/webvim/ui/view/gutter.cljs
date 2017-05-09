(ns webvim.ui.view.gutter
  (:require
    [clojure.string :as string]
    [webvim.ui.lib.dom :refer [$id $text-content $hiccup $lines client-size measure-text-size]]))

(defn render-gutter [{old-lines :lines}
                     {lines :lines
                      bufid :id
                      [_ current] :cursor}]
  (if (not= old-lines lines)
    (let [[_ ph] (client-size)
          [_ ch] (measure-text-size "M")
          page-lines (quot (- ph (* 1.5 ch)) ch)
          margin-lines (max (- page-lines 1) 0)
          $g ($id (str "gutter-" bufid))
          $lastChild (.-lastChild $g)
          max (if (some? $lastChild)
                (-> $lastChild .-textContent js/parseInt) 0)]
      (if (< max lines)
        (dotimes [i (- lines max)]
          (let [num (+ i max)]
            (.appendChild $g
                          ($hiccup
                            [:div.line-num
                             {:id (str "line-num-" bufid "-" num)}
                             (inc num)])))))
      (if (> max lines)
        (dotimes [_ (- max lines)]
          (-> $g .-lastChild .remove)))
      (-> ($lines bufid)
          .-style
          .-height
          (set! (str (* ch (+ lines margin-lines)) "px"))))))
