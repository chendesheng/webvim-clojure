(ns webvim.ui.view
  (:require [webvim.ui.lib.dom :refer [$id $hiccup $exist? add-class remove-class $remove $text-content beep $empty measure-text-size $show $hide line-height]]
            [webvim.ui.lib.patch :refer [trigger-patch]]
            [webvim.ui.lib.util :refer [deep-merge]]
            [webvim.ui.lib.event :refer [add-listener]]
            [webvim.fuzzy :refer [fuzzy-match]]
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
                 [:div.cursor.cursor2 {:id (str "cursor2-" bufid)}]
                 [:div.selections {:id (str "selections-" bufid)}]
                 [:div.highlights {:id (str "highlights-" bufid)}]]]))))

(defn- $layouts [layouts]
  ;(println "layouts:" layouts)
  ($hiccup (conj layouts {:id "buffers"})
           (fn [k]
             ($hiccup 
               [(cond
                  (= k "-") :div.ver
                  (= k "|") :div.hor
                  ;should not happen
                  :else :div)]))
           (fn [ele bufid]
             ;(js/console.log ele)
             (.appendChild ele ($buffer bufid)))))

(def normal-mode 0)
(def insert-mode 1)
(def ex-mode 2)

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

(defn- bounding-rect
  ([ele a b]
    (.getBoundingClientRect (doto (js/document.createRange)
                              (.setStart ele a)
                              (.setEnd ele b))))
  ([ele pos]
    (let [rects (.getClientRects (doto (js/document.createRange)
                                   (.setStart ele pos)
                                   (.setEnd ele pos)))]
      (aget rects (-> rects .-length dec))))
  ([ele]
    (.getBoundingClientRect ele)))

(defn- rect-pos [rect]
  [(.-left rect) (.-top rect)])

(defn- rect-size [rect]
  [(.-width rect) (.-height rect)])

(defn- render-status-bar-cursor [$statusbuf $cur pos]
  (let [$statusbuf ($id "status-bar-buf")
        left (if (nil? pos)
               -100
               (.-left (bounding-rect (.-firstChild $statusbuf) pos)))]

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

(defn- cursor-position [$lines-nodes px py]
  (let [$cur-line (aget $lines-nodes py)]
    (if (some? $cur-line)
      (let [rect (-> $cur-line
                     .-firstChild
                     (bounding-rect px))
            [x y] (rect-pos rect)
            h (.-height rect)]
        ;(println "xy:" x y)
        [x (dec y) h]))))

(defn- cursor-position-in-buffer [$lines cx cy]
  (let [[linesx linesy :as lines-pos] (rect-pos (bounding-rect $lines))
        [px py h] (or (cursor-position (.-childNodes $lines) cx cy) lines-pos)]
    [(- px linesx) (- py linesy) h]))

(defn- render-cursor [$lines $cur cx cy reverse-background?]
  (let [$cur-line (aget (.-childNodes $lines) cy)
        [px py] (cursor-position-in-buffer $lines cx cy)]
    (if reverse-background?
      (let [ch (if (some? $cur-line)
                 (.substr (.-textContent $cur-line) cx 1) " ")]
        ($text-content $cur ch)
        (doto (.-style $cur)
          (-> .-background (set! "#fff"))
          (-> .-color (set! "#000"))
          (-> .-width (set! ""))
          (-> .-height (set! "")))
        (if (string/blank? ch)
          (-> $cur .-style .-width (set! "1ch"))))
      (doto $cur
        (-> .-textContent (set! ""))
        (-> .-style .-width (set! "1ch"))
        (-> .-style .-height (set! (str (line-height) "px")))))
    (doto (.-style $cur)
      (-> .-left (set! (str px "px")))
      (-> .-top (set! (str py "px"))))))

(defn- lines-ranges [$lines [[ax ay] [bx by]]]
  (let [$lines-nodes (.-childNodes $lines)
        lines (reduce (fn [lines i]
                        (let [$line (aget $lines-nodes i)
                              length (-> $line .-textContent count)]
                          (conj lines [[0 i] [length i]]))) [] (range ay (inc by)))]
    (-> lines
        (assoc-in [0 0 0] ax)
        (assoc-in [(- by ay) 1 0] bx))))

(defn- render-highlight [$lines $highlights rg]
  (println "render-highlight")
  (println rg)
  (doseq [[[ax ay] [bx by]] (lines-ranges $lines rg)]
    (println "range:" ax ay bx by)
    (let [rt (-> $lines .-childNodes (aget ay) .-firstChild (bounding-rect ax bx))
          [x y] (let [[linesx linesy] (rect-pos (bounding-rect $lines))
                      [x y] (rect-pos rt)]
                  [(- x linesx) (- y linesy)])
          [w h] (rect-size rt)]
      (.appendChild $highlights
                    ($hiccup [:span.line-selected
                              {:style (str "left:" x "px;" "top:" y "px;"
                                           "width:" w "px;" "height:" (line-height) "px;"
                                           "padding-right:1ch;")}])))))

(defn render [patch _ _]
  ;(println "ROOT")
  ;(println patch)
  (if (not ($exist? "editor")) ;global ui
    (.appendChild js/document.body
                  ($hiccup [:div#editor
                            [:div#buffers]
                            [:div#status-bar.status-bar
                             [:span#status-bar-buf.ex]
                             [:span#status-bar-cursor.cursor]
                             [:span#status-bar-cursor-second.cursor.cursor-second]
                             [:span#status-bar-keys.ongoing-keys]
                             [:span#status-bar-name.buf-name]]
                            [:div#autocompl.autocompl]])))
  {:title (fn [_ [{cwd :cwd name :name}] _]
            (set! js/document.title (str name " - " cwd)))
   :layouts (fn [layouts _ _]
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
                            ;(println "showkeys:" showkeys)
                            (let [$keys ($id "status-bar-keys")]
                              ($text-content $keys (string/join "" (reverse showkeys)))
                              (if (-> showkeys first nil?)
                                (js/setTimeout #($text-content $keys "") 100))))
                :name (fn [name _ _]
                        ($text-content ($id "status-bar-name") name))
                :beep? (fn [beep? _ _]
                         (println "beep:" beep?)
                         (if beep? (beep)))
                :dirty? (fn [dirty? _ _]
                          (toggle-class ($id "status-bar-name") "dirty" dirty?))}
   :autocompl (fn [_ [{sugs :suggestions i :index ex-autocompl? :ex-autocompl? bufid :bufid [px py] :cursor}] _]
                ;(println "render autocompl")
                (if (-> sugs count (> 1))
                  (let [$autocompl ($id "autocompl")
                        subject (-> sugs first :name)
                        selected-sug ((nth sugs i) :name)]
                    ($empty $autocompl)
                    (doseq [{nm :name cls :class} (rest sugs)]
                      (.appendChild $autocompl 
                                    (let [$item ($hiccup [:pre.with-class {:class cls}])
                                          indexes (fuzzy-match nm subject)]
                                      (println indexes)
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
                    ((if ex-autocompl?
                       add-class remove-class) $autocompl "ex-autocompl")
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
                  ($hide ($id "autocompl"))))
   :buffers {:*
             {:content (fn [{changes :changes} [{[cx cy] :cursor} {bufid :id} :as new-path] _]
                         ;(println "changes")
                         ;(println changes)
                         (let [$lines ($id (str "lines-" bufid))
                               $lines-nodes (.-childNodes $lines)]
                           (if-not (empty? changes)
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
                                     (.appendChild $lines ($hiccup [:span.code-block line])))))))
                           (render-cursor $lines ($id (str "cursor-" bufid)) cx cy true)
                           {:cursor2 (fn [[cx cy :as cursor2] [_ {cursor :cursor} _] _]
                                       (let [$lines-nodes ($id (str "lines-" bufid))
                                             $cursor2 ($id (str "cursor2-" bufid))
                                             show? (not (or (empty? cursor2) (= cursor2 cursor)))]
                                         ((if show?
                                            $show $hide) $cursor2)
                                         (if show? 
                                           (render-cursor $lines $cursor2 cx cy false))))
                            :highlights (fn [highlights _ _]
                                          (println "render highlights")
                                          (let [$highlights ($id (str "highlights-" bufid))]
                                            ($empty $highlights)
                                            (doseq [rg highlights]
                                              (println "highlight range:" rg)
                                              (render-highlight $lines $highlights rg))))
                            :visual (fn [{ranges :ranges} _ _]
                                      (println "render highlights")
                                      (let [$selections ($id (str "selections-" bufid))]
                                        ($empty $selections)
                                        (doseq [rg ranges]
                                          (render-highlight $lines $selections rg))))}))
              :scroll-top (fn [scroll-top [_ {bufid :id}] _]
                            (set! (.-scrollTop ($id (str "buffer-" bufid)))
                                  (* scroll-top (line-height))))
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
              (and (map? v) (empty? v)))
    (assoc coll k v)
    coll))

(defn- generate-ui-patch [patch new-client old-client]
  ;(println "old-client:" old-client)
  ;(println "patch:" patch)
  ;(println "new-client:" new-client)
  (let [active-changed? (and (-> patch :active-buf nil? not)
                             (not= (patch :active-buf) (old-client :active-buf)))
        new-active (-> new-client :buffers (get (new-client :active-buf)))]
    ;(println "new-active:" new-active)
    (-> {}
        (try-assoc :layouts (patch :layouts))
        (try-assoc :title (-> patch
                              (select-keys [:cwd])
                              (try-assoc :name (if active-changed?
                                                 (:name new-active)))))
        (try-assoc :status-bar (let [buf (if active-changed?
                                           new-active
                                           (-> patch :buffers (get (:id new-active))))]
                                 (-> buf
                                     (select-keys [:showkeys :dirty? :name :lang :line-buffer :message])
                                     (assoc :focus? (-> new-active :line-buffer nil? not))
                                     (try-assoc :beep? (buf :beep))
                                     (try-assoc :mode
                                                (-> (select-keys buf [:mode :submode])
                                                    (try-assoc :visual-type (-> buf :visual :type)))))))
        (try-assoc :buffers (reduce-kv
                              (fn [buffers bufid buf]
                                (assoc buffers bufid
                                       (-> {}
                                           (assoc :id bufid)
                                           (try-assoc :focus? (if active-changed? (= bufid (:id new-active))))
                                           (try-assoc :content (-> (select-keys buf [:changes :cursor :cursor2 :visual :tabsize])
                                                                   (try-assoc :highlights (buf :highlights2))
                                                                   (try-assoc :visual (if (buf :visual)
                                                                                        {:type (-> buf :visual :type)
                                                                                         :ranges (-> buf :visual :ranges2)}))))
                                           (try-assoc :gutter (select-keys buf [:scroll-top :lines]))
                                           (try-assoc :scroll-top (buf :scroll-top))))) {} (patch :buffers)))
        (assoc :autocompl
               (let [autocompl (-> patch :buffers (get (:id new-active)) :autocompl)]
                 (if (some? autocompl)
                   (let [ex-mode? (-> new-active :mode (= ex-mode))]
                     (-> autocompl
                         (assoc :ex-autocompl? ex-mode?)
                         (assoc :bufid (new-active :id))
                         (try-assoc :cursor (when-not ex-mode?
                                              (new-active :cursor)))))))))))

(def ^:private ui (atom nil))

(add-listener :client-changed :ui-render
              (fn [[patch old-client new-client]]
                (let [patch (generate-ui-patch patch new-client old-client)
                      old-ui @ui
                      new-ui (swap! ui deep-merge patch)] 
                  ;(println "ui-patch:" patch)
                  (trigger-patch render patch (list new-ui) (list old-ui)))))
