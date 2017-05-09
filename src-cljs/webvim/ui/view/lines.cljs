(ns webvim.ui.view.lines
  (:require
    [clojure.string :as string]
    [webvim.ui.lib.dom :refer [bounding-rect $id $text-content toggle-class $hiccup]]))

(defn- split-lines [s]
  (loop [lines []
         s s]
    (let [p (inc (.indexOf s "\n"))]
      (if (pos? p)
        (recur (conj lines (.substr s 0 p)) (.substr s p))
        (if-not (empty? s)
          (conj lines s) lines)))))

(defn render-lines [{old-changes :changes}
                    {changes :changes
                     bufid :id}]
  (if (and (not= old-changes changes)
           (not (empty? changes)))
    (let [$lines ($id (str "lines-" bufid))
          $lines-nodes (.-childNodes $lines)]
      (doseq [{[xa ya] :a [xb yb] :b to :to} changes]
        (let [$linea (aget $lines-nodes ya)
              $lineb (aget $lines-nodes yb)
              lines (split-lines
                      (str
                        (if (some? $linea)
                          (-> $linea .-textContent (.substr 0 xa)))
                        to
                        (if (some? $lineb)
                          (-> $lineb .-textContent (.substr xb)))))]
          ;(println lines)
          ;(println "xa" xa "ya" ya)
          ;(println "xb" xb "yb" yb)
          ;(println "$linea")
          ;(js/console.log $linea)
          (dotimes [_ (inc (- yb ya))]
            (if-let [$linea (aget $lines-nodes ya)]
              (.remove $linea)))
          (if-let [after (aget $lines-nodes ya)]
            (doseq [line lines]
              (.insertBefore $lines ($hiccup [:span.code-block line]) after))
            (doseq [line lines]
              (.appendChild $lines ($hiccup [:span.code-block line])))))))))
