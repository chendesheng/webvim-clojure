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

(def scope-cache (array))

(def hljs js/window.hljs)

(defn highlight [lang line top container]
  (if (-> hljs (.getLanguage lang) nil?)
    (set! (.-textContent container) line)
    (let [result (.highlight hljs lang line true top)
          html (.-value result)
          nexttop (.-top result)]
      (set! (.-innerHTML container) html)
      nexttop)))

(defn- normalize-classes [classes]
  (->> classes
       (mapcat (fn [class-name]
                 (string/split class-name ".")))
       distinct
       ;(filter (complement #{"source" "clojure"}))
       (string/join " ")))

(defn- create-span [text class-name]
  (doto (js/document.createElement "SPAN")
    (-> .-className (set! class-name))
    (-> .-textContent (set! text))))

(defn- render-line [line scopes]
  (loop [i 0
         [[a b classes :as scope] & scopes] scopes
         result ($hiccup [:span.code-block])]
    (if (some? scope)
      (do
        (if (< i a) (.appendChild result (js/document.createTextNode (subs line i a))))
        (.appendChild result
                      (create-span (subs line a b)
                                   (normalize-classes classes)))
        (recur b scopes result))
      result)))

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

(defn render-scopes [{old-scope-changes :scope-changes} {scope-changes :scope-changes bufid :id}]
  (if (and (not= old-scope-changes scope-changes)
           (-> scope-changes empty? not))
    (let [[start scopes] scope-changes
          $lines ($id (str "lines-" bufid))]
      (loop [$line (-> $lines .-childNodes (aget start))
             [scope & scopes] scopes]
        (if (some? scope)
          (let [line ($text-content $line)
                $next-line (.-nextSibling $line)]
            (.replaceChild $lines
                           (render-line line scope)
                           $line)
            (recur $next-line scopes)))))))

