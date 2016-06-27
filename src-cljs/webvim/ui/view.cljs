(ns webvim.ui.view
  (:require [webvim.ui.lib.dom :refer [$id $hiccup $exist? add-class remove-class $remove $text-content beep]]
            [webvim.ui.lib.patch :refer [trigger-patch]]
            [webvim.ui.lib.util :refer [deep-merge]]
            [webvim.ui.lib.event :refer [add-listener]]
            [clojure.string :refer [split-lines]]))

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

(defn measure-left [ele pos]
  (let [range (doto (js/document.createRange)
                (.setStart ele (dec pos))
                (.setEnd ele pos))
        rect (.getBoundingClientRect range)]
    (str (+ (.-left rect) (.-width rect)) "px")))

(defn- render-status-bar-cursor [$statusbuf $cur pos]
  (let [$statusbuf ($id "status-bar-buf")
        left (if (nil? pos)
               "-100px"
               (measure-left (.-firstChild $statusbuf) pos))]
    (set! (-> $cur .-style .-left) left)))

(defn- toggle-class [ele cls b]
  ((if b add-class remove-class)
    ele cls))

(defn render [patch _ _]
  (println "ROOT")
  (println patch)
  (if (not ($exist? "editor")) ;global ui
    (.appendChild js/document.body
                  ($hiccup [:div#editor
                            [:div#status-bar.status-bar
                             [:span#status-bar-buf.ex]
                             [:span#status-bar-cursor.cursor]
                             [:span#status-bar-cursor-second.cursor.cursor-second]
                             [:span#status-bar-keys.ongoing-keys]
                             [:span#status-bar-name.buf-name]]])))
  {:title (fn [_ [{cwd :cwd name :name}] _]
            (set! js/document.title (str name " - " cwd)))
   :layouts (fn [layouts _ _]
              (let [ele ($layouts layouts)]
                ($remove "buffers")
                (.appendChild ($id "editor") ele))) 
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
                              ($text-content $keys (clojure.string/join "" (reverse showkeys)))
                              (if (-> showkeys first nil?)
                                (js/setTimeout #($text-content $keys "") 100))))
                :name (fn [name _ _]
                        ($text-content ($id "status-bar-name") name))
                :beep? (fn [beep? _ _] (if beep? (beep)))
                :dirty? (fn [dirty? _ _]
                          (toggle-class ($id "status-bar-name") "dirty" dirty?))}
   :buffers {:*
             {:content (fn [content [_ {bufid :id} :as new-path] _]
                         {:changes (fn [changes _ _]
                                     (println "new-path:" new-path)
                                     (doseq [{pos :pos len :len to :to} changes]
                                       ($text-content ($id (str "lines-" bufid)) to)))})
              :gutter (fn [gutter new-path old-path]
                        (println "buffers.*.gutter"))}}})

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
        (try-assoc :layouts (if (-> new-client :layouts nil?)
                              (into [:|] (-> new-client :buffers keys))
                              (select-keys patch [:layouts])))
        (try-assoc :title (-> patch
                              (select-keys [:cwd])
                              (try-assoc :name (if active-changed?
                                                 (:name new-active)))))
        (try-assoc :status-bar (let [buf (if active-changed?
                                           new-active
                                           (-> patch :buffers (get (:id new-active))))]
                                 (-> buf
                                     (select-keys [:showkeys :dirty? :beep? :name :lang :line-buffer :message])
                                     (assoc :focus? (-> buf :line-buffer nil? not))
                                     (try-assoc :mode
                                                (-> (select-keys buf [:mode :submode])
                                                    (try-assoc :visual-type (-> buf :visual :type)))))))
        (try-assoc :buffers (reduce-kv
                              (fn [buffers bufid buf]
                                (let [str (buf :str)
                                      buf (if-not (nil? str)
                                            (-> buf
                                                (assoc :changes [{:pos 0 :len 0 :to str}])
                                                (dissoc :str)))] ;TODO: get rid of :str on server side
                                  (assoc buffers bufid
                                         (-> {}
                                             (assoc :id bufid)
                                             (try-assoc :focus? (if active-changed? (= bufid (:id new-active))))
                                             (try-assoc :content (select-keys buf [:changes :scroll-top :y :pos :highlights :visual :tabsize :brackets]))
                                             (try-assoc :gutter (select-keys buf [:y :scroll-top])))))) {} (patch :buffers))))))

(def ^:private ui (atom nil))

(add-listener :client-changed :ui-render
              (fn [[patch old-client new-client]]
                (let [patch (generate-ui-patch patch new-client old-client)
                      old-ui @ui
                      new-ui (swap! ui deep-merge patch)] 
                  (println "ui-patch:" patch)
                  (trigger-patch render patch (list new-ui) (list old-ui)))))
