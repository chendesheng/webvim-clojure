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

;; TODO: put cache into buffer object
(def scope-cache (array))

(def hljs js/window.hljs)

(defn highlight [line top container]
  (let [result (.highlight hljs "clojure" line true top)
        html (.-value result)
        nexttop (.-top result)]
    (set! (.-innerHTML container) html)
    nexttop))

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
            (when-let [$linea (aget $lines-nodes ya)]
              (.remove $linea)
              (.splice scope-cache ya 1)))
          (if-let [after (aget $lines-nodes ya)]
            (loop [lines lines
                   index ya ; index of the "after" line
                   top (aget scope-cache (dec ya))]
              (if (not-empty lines)
                (let [line (first lines)
                      $line ($hiccup [:span.code-block])
                      nexttop (highlight line top $line)]
                  (.insertBefore $lines $line after)
                  (.splice scope-cache index 0 nexttop)
                  (recur (rest lines) (inc index) nexttop))
                (let [currenttop (aget scope-cache index)]
                  (when (not= top currenttop)
                    (aset scope-cache index top)
                    (if (-> $lines-nodes .-length (> index))
                      (let [$new-line ($hiccup [:span.code-block])
                            $line (aget $lines-nodes index)
                            line (.-textContent $line)
                            nexttop (highlight line top $new-line)]
                        (.replaceChild $lines $new-line $line)
                        (recur nil (inc index) nexttop)))))))
            (loop [lines lines
                   index (.-length $lines-nodes)
                   top (aget scope-cache (.-length $lines-nodes))]
              (if (not-empty lines)
                (let [line (first lines)
                      $line ($hiccup [:span.code-block])
                      nexttop (highlight line top $line)]
                  (aset scope-cache index nexttop)
                  (.appendChild $lines $line)
                  (recur (rest lines) (inc index) nexttop))))))))))
