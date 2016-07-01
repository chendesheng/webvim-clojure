(ns webvim.ui.view
  (:require [webvim.ui.lib.dom :refer [$id $hiccup $exist? add-class remove-class $remove $text-content beep $empty measure-text-size]]
            [webvim.ui.lib.patch :refer [trigger-patch]]
            [webvim.ui.lib.util :refer [deep-merge]]
            [webvim.ui.lib.event :refer [add-listener]]
            [clojure.string :as string]))

(defn- $buffer [bufid]
  (let [domid (str "buffer-" bufid)
        ele ($id domid)]
    (if ele
      ($remove ele)
      ($hiccup [:div.buffer {:id domid}
                [:div.gutter {:id (str "gutter-" bufid)}]
                [:div.content {:id (str "content-" bufid)}
                 [:div.lines {:id (str "lines-" bufid)}]
                 [:div.cursor {:id (str "cursor-" bufid)}]
                 [:div.selections {:id (str "selections-" bufid)}]
                 [:div.highlights {:id (str "highlights-" bufid)}]
                 [:div.brackets {:id (str "brackets-" bufid)}]
                 [:div.autocompl.ex-autocompl {:id (str "ex-autocompl-" bufid)}]]]))))

(defn- $layouts [layouts]
  (println "layouts:" layouts)
  ($hiccup (conj layouts {:id "buffers"})
           (fn [k]
             ($hiccup 
               [(cond
                  (= k "-") :div.ver
                  (= k "|") :div.hor
                  ;should not happen
                  :else :div)]))
           (fn [ele bufid]
             (js/console.log ele)
             (.appendChild ele ($buffer bufid)))))

(defn- mode-text [mode submode visual-type]
  (let [modes ["NORMAL" "INSERT"]
        submodes ["" "(insert)"];
        visual-types ["" "VISUAL" "VISUAL LINE" "VISUAL BLOCK"]]
    (if (< mode (count modes))
      (str "-- "
           (cond
             (and (zero? submode) (zero? visual-type))
             (modes mode)
             (and (zero? submode) (pos? visual-type))
             (visual-types visual-type)
             (pos? submode) 
             (str (submodes submode)
                  (if (pos? visual-type)
                    (str " " (visual-types visual-type)))))
           " --"))))

(defn- bounding-rect [ele start end]
  (.getBoundingClientRect (doto (js/document.createRange)
                            (.setStart ele start)
                            (.setEnd ele end))))

(defn- rect-left-top [rect]
  [(.-left rect) (.-top rect)])

(defn- render-status-bar-cursor [$statusbuf $cur pos]
  (let [$statusbuf ($id "status-bar-buf")
        left (if (nil? pos)
               -100
               (.-left (bounding-rect (.-firstChild $statusbuf) pos pos)))]
    (set! (-> $cur .-style .-left) (str (dec left) "px"))))

(defn- toggle-class [ele cls b]
  ((if b add-class remove-class)
    ele cls))

;;keep line break
(defn- split-lines [s]
  (loop [lines []
         s s]
    (let [p (inc (.indexOf s "\n"))]
      (if (pos? p)
        (recur (conj lines (.substr s 0 p)) (.substr s p))
        (if-not (empty? s)
          (conj lines s) lines)))))

(defn- normalize-cursor [children x y]
  (let [$line (aget children y)]
    (if (and (some? $line)
             (-> $line .-length (= x)))
      [0 (inc y)] [x y])))

(defn render [patch _ _]
  (println "ROOT")
  (println patch)
  (if (not ($exist? "editor")) ;global ui
    (.appendChild js/document.body
                  ($hiccup [:div#editor
                            [:div#buffers]
                            [:div#status-bar.status-bar
                             [:span#status-bar-buf.ex]
                             [:span#status-bar-cursor.cursor]
                             [:span#status-bar-cursor-second.cursor.cursor-second]
                             [:span#status-bar-keys.ongoing-keys]
                             [:span#status-bar-name.buf-name]]])))
  {:title (fn [_ [{cwd :cwd name :name}] _]
            (set! js/document.title (str name " - " cwd)))
   :layouts (fn [{layouts :layouts} _ _]
              (.replaceChild ($id "editor")
                             ($layouts layouts)
                             ($id "buffers")))
   :status-bar {:message (fn [message _ _]
                           ($text-content ($id "status-bar-buf") message))
                :mode (fn [_ [{mode :mode submode :submode visual-type :visual-type} {message :message}] _]
                        (if (empty? message)
                          (let [text (mode-text mode submode visual-type)]
                            (if-not (empty? text)
                              ($text-content ($id "status-bar-buf") text)))))
                :focus? (fn [focus? _ _]
                          (toggle-class ($id "status-bar") "focus" focus?))
                :line-buffer (fn [_ [{str :str pos :pos pos2 :pos2}] _]
                               (let [$statusbuf ($id "status-bar-buf")]
                                 ($text-content $statusbuf str)
                                 (render-status-bar-cursor $statusbuf ($id "status-bar-cursor") pos)
                                 (render-status-bar-cursor $statusbuf ($id "status-bar-cursor-second") pos2)))
                :showkeys (fn [showkeys _ _]
                            (println "showkeys:" showkeys)
                            (let [$keys ($id "status-bar-keys")]
                              ($text-content $keys (string/join "" (reverse showkeys)))
                              (if (-> showkeys first nil?)
                                (js/setTimeout #($text-content $keys "") 100))))
                :name (fn [name _ _]
                        ($text-content ($id "status-bar-name") name))
                :beep? (fn [beep? _ _] (if beep? (beep)))
                :dirty? (fn [dirty? _ _]
                          (toggle-class ($id "status-bar-name") "dirty" dirty?))}
   :buffers {:*
             {:content (fn [{changes :changes} [{[px py] :cursor} {bufid :id} :as new-path] _]
                         (println "changes")
                         (println changes)
                         (let [$lines ($id (str "lines-" bufid))
                               children (.-childNodes $lines)]
                           (if-not (empty? changes)
                             (doseq [{[xa ya] :a [xb yb] :b to :to} changes]
                               (let [$linea (aget children ya)
                                     $lineb (aget children yb)
                                     lines (split-lines
                                             (str
                                               (if (some? $linea)
                                                 (-> $linea .-textContent (.substr 0 xa)))
                                               to
                                               (if (some? $lineb)
                                                 (-> $lineb .-textContent (.substr xb)))))]
                                 (println lines)
                                 (println "xa" xa "ya" ya)
                                 (println "xb" xb "yb" yb)
                                 (println "$linea")
                                 (js/console.log $linea)
                                 (dotimes [_ (inc (- yb ya))]
                                   (if-let [$linea (aget children ya)]
                                     (.remove $linea)))
                                 (if-let [after (aget children ya)] 
                                   (doseq [line lines]
                                     (.insertBefore $lines ($hiccup [:div.code-block line]) after))
                                   (doseq [line lines]
                                     (.appendChild $lines ($hiccup [:div.code-block line])))))))
                           (let [$cur ($id (str "cursor-" bufid))
                                 $cur-line (aget children py)
                                 _ (js/console.log $cur-line)
                                 [linesx linesy] (rect-left-top (.getBoundingClientRect $lines))
                                 [left top] (if (some? $cur-line)
                                              (let [[x y] (-> $cur-line
                                                              .-firstChild
                                                              (bounding-rect px px)
                                                              rect-left-top)]
                                                (println "xy:" x y)
                                                [(- x linesx) (- y linesy)])
                                              [0 0])]
                             (println linesx linesy)
                             (println left top)
                             (let [ch (if (some? $cur-line)
                                        (.substr (.-textContent $cur-line) px 1) " ")]
                               ($text-content $cur ch)
                               (if (string/blank? ch)
                                 (-> $cur .-style .-width (set! "1ch"))))
                             (doto (.-style $cur)
                               ;(-> .-marginLeft (set! "-5ch"))
                               (-> .-color (set! "#000"))
                               (-> .-background (set! "#fff"))
                               (-> .-left (set! (str left "px")))
                               (-> .-top (set! (str (dec top) "px")))))))
              :scroll-top (fn [scroll-top [_ {bufid :id}] _]
                            (set! (.-scrollTop ($id (str "buffer-" bufid)))
                                  (* scroll-top (last (measure-text-size "M")))))
              :gutter (fn [_ [{lines :lines} {bufid :id}] _]
                        (let [$g ($id (str "gutter-" bufid))
                              $lastChild (.-lastChild $g)
                              max (if (some? $lastChild)
                                    (-> $lastChild .-textContent js/parseInt) 0)]
                          (if (< max lines)
                            (dotimes [i (- lines max)]
                              (.appendChild $g ($hiccup [:div.line-num (+ i max 1)]))))
                          (if (> max lines)
                            (dotimes [_ (- max lines)]
                              (-> $g .-lastChild .remove)))))}}})

(defn- try-assoc [coll k v]
  (if-not (or (nil? v)
              (and (or (coll? v) (seq? v))
                   (empty? v)))
    (assoc coll k v)
    coll))

(defn- generate-ui-patch [patch new-client old-client]
  (println "old-client:" old-client)
  (println "patch:" patch)
  (println "new-client:" new-client)
  (let [active-changed? (and (-> patch :active-buf nil? not)
                             (not= (patch :active-buf) (old-client :active-buf)))
        new-active (-> new-client :buffers (get (new-client :active-buf)))]
    (println "new-active:" new-active)
    (-> patch {}
        (try-assoc :layouts  (select-keys patch [:layouts]))
        (try-assoc :title (-> patch
                              (select-keys [:cwd])
                              (try-assoc :name (if active-changed?
                                                 (:name new-active)))))
        (try-assoc :status-bar (let [buf (if active-changed?
                                           new-active
                                           (-> patch :buffers (get (:id new-active))))]
                                 (-> buf
                                     (select-keys [:showkeys :dirty? :beep? :name :lang :line-buffer :message])
                                     (assoc :focus? (-> new-active :line-buffer nil? not))
                                     (try-assoc :mode
                                                (-> (select-keys buf [:mode :submode])
                                                    (try-assoc :visual-type (-> buf :visual :type)))))))
        (try-assoc :buffers (reduce-kv
                              (fn [buffers bufid buf]
                                (assoc buffers bufid
                                       (-> buf 
                                           (select-keys [:scroll-top])
                                           (assoc :id bufid)
                                           (try-assoc :focus? (if active-changed? (= bufid (:id new-active))))
                                           (try-assoc :content (select-keys buf [:changes :cursor :highlights :visual :tabsize :brackets]))
                                           (try-assoc :gutter (select-keys buf [:scroll-top :lines]))))) {} (patch :buffers))))))

(def ^:private ui (atom nil))

(add-listener :client-changed :ui-render
              (fn [[patch old-client new-client]]
                (let [patch (generate-ui-patch patch new-client old-client)
                      old-ui @ui
                      new-ui (swap! ui deep-merge patch)] 
                  (println "ui-patch:" patch)
                  (trigger-patch render patch (list new-ui) (list old-ui)))))
