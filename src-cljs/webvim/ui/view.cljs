(ns webvim.ui.view
  (:require [webvim.ui.lib.dom :refer [$id $hiccup $exist? add-class remove-class
                                       $remove $text-content beep $empty $hidden-input
                                       $status-bar $status-bar-cursor $buffer $view $exist?
                                       measure-text-size $show $hide line-height $cursor]]
            [webvim.ui.lib.event :refer [add-listener]]
            [webvim.fuzzy :refer [fuzzy-match]]
            [webvim.ui.view.statusbar :refer [render-status-bar]]
            [webvim.ui.view.gutter :refer [render-gutter]]
            [webvim.ui.view.lines :refer [render-lines]]
            [webvim.ui.view.cursor :refer [render-cursor]]
            [webvim.ui.view.autocompl :refer [render-autocompl]]
            [webvim.ui.view.visual :refer [render-visual render-highlights]]
            [clojure.string :as string]))

(defn- tprintln [x]
  (println x)
  x)

(defn- tlog [x]
  (js/console.log x)
  x)

(defn- diff? [o1 o2 key]
  (not= (get o1 key) (get o2 key)))

(defn- active-buffer [{active :active-buffer buffers :buffers}]
  ;(println (keys buffers))
  (if (some? buffers)
    (buffers active)))

(defn- render-beep [old-buf buf]
  (if (buf :beep?) (beep)))

(defn- render-title [{old-cwd :cwd} {old-name :name} {cwd :cwd} {name :name}]
  (if (or (not= old-cwd cwd)
          (not= old-name name))
    (set! js/document.title (str name " - " cwd))))

(defn- render-buffer [{old-bufid :id} {bufid :id view :view}]
  ;(println "render-buffer:")
  (when (and (some? old-bufid) (nil? bufid))
    ;(println "old-bufid:" old-bufid)
    ;(println "old-bufid:" ($buffer old-bufid))
    ;(println "old-bufid:" ($status-bar old-bufid))
    (doto old-bufid
      (-> $buffer $remove)
      (-> $status-bar $remove)))
  (when (and (nil? old-bufid)
             (some? bufid)
             (-> bufid $buffer nil?))
    ;(println ($view view))
    (doto ($view view)
      (.appendChild ($hiccup
                      [:div.buffer {:id (str "buffer-" bufid)}
                       [:div.gutter {:id (str "gutter-" bufid)}]
                       [:div.content {:id (str "content-" bufid)}
                        [:div.lines {:id (str "lines-" bufid)}]
                        [:div.cursor {:id (str "cursor-" bufid)} " "]
                        [:div.cursor.cursor2 {:id (str "cursor2-" bufid)} " "]
                        [:div.selections {:id (str "selections-" bufid)}]
                        [:div.highlights {:id (str "highlights-" bufid)}]]]))
      (.appendChild ($hiccup
                      [:div.status-bar {:id (str "status-bar-" bufid)}
                       [:span.ex {:id (str "status-bar-buf-" bufid)}]
                       [:span.cursor {:id (str "status-bar-cursor-" bufid)}]
                       [:span.cursor.cursor-second {:id (str "status-bar-cursor-second-" bufid)}]
                       [:span.buf-name {:id (str "status-bar-name-" bufid)}]
                       [:span.ongoing-keys {:id (str "status-bar-keys-" bufid)}]])))
    (->> ($hidden-input) $remove (.appendChild ($cursor bufid)))))

(defn- render-scroll-top [{old-scroll-top :scroll-top}
                          {scroll-top :scroll-top bufid :id}]
  (if (not= old-scroll-top scroll-top)
    (set! (.-scrollTop ($buffer bufid))
          (* scroll-top (line-height)))))

(def normal-mode 0)
(def insert-mode 1)
(def ex-mode 2)

(defn- render-ime [{old-bufid :id
                    old-mode :mode
                    old-line-buffer :line-buffer}
                   {bufid :id
                    mode :mode
                    line-buffer :line-buffer}]
  (let [$input ($hidden-input)]
    ; (println "render-ime" $input bufid)
    (when (and (nil? old-line-buffer)
               (some? line-buffer))
      (doto ($hidden-input)
        ($remove)
        (->> (.appendChild ($status-bar-cursor bufid)))))
    (when (and (some? old-line-buffer)
               (nil? line-buffer))
      (doto ($hidden-input)
        ($remove)
        (->> (.appendChild ($cursor bufid)))))
    (cond
      (or (= mode insert-mode)
          (some? line-buffer))
      (doto $input
        (-> .-disabled (set! false))
        (.focus))
      (= mode normal-mode)
      (doto $input
        (.blur)
        (-> .-disabled (set! true))))))

(defn- percent [x]
  (-> x (* 100) (str "%")))

(defn- render-views-recur [view direction size]
  (let [style {:style (str (name direction) ":" (percent size))}]
    (if (vector? view)
      (let [[type size viewa viewb] view]
        (cond
          (= type "-")
          [:div.view.view-box.view-box-hor style
           (render-views-recur viewa :height size)
           (render-views-recur viewb :height (- 1 size))]
          (= type "|")
          [:div.view.view-box.view-box-ver style
           (render-views-recur viewa :width size)
           (render-views-recur viewb :width (- 1 size))]))
      [:div.view.inactive-view (assoc style :id (str "view-" view))])))

(defn- render-views [{old-views :views} {views :views buffers :buffers}]
  (if (not= old-views views)
    (let [buffers (vals buffers)
          $newviews ($hiccup (render-views-recur views :width 1.0))
          tmpdoc (js/document.createDocumentFragment)]
      (.appendChild tmpdoc $newviews)
      (js/console.log "buffers:" buffers)
      (doseq [{bufid :id view :view} buffers]
        (let [$old-v ($view view)
              $v (.getElementById tmpdoc (str "view-" view))]
          ;(println "render-views:" bufid view)
          ;(println $old-v)
          ;(println $v)
          (if (and (some? $old-v)
                   (some? $v))
            (loop [$ele (.-firstChild $old-v)]
              (if (some? $ele)
                (do
                  (->> $ele
                       $remove
                       (.appendChild $v))
                  (recur (.-firstChild $old-v))))))))
      (doto ($id "buffers")
        ($empty)
        (.appendChild (-> tmpdoc .-firstChild $remove))))))

(defn- render-active-buffer [old-client client]
  (let [{old-view :view old-bufid :id :as old-buf} (active-buffer old-client)
        {view :view bufid :id :as buf} (active-buffer client)]
    (when (not= old-view view)
      (if (some? old-view)
        (-> old-view $view (add-class "inactive-view")))
      (if (some? view)
        (-> view $view (remove-class "inactive-view"))))
    ;(println "render-active-buffer:" old-bufid bufid)
    (when (not= old-bufid bufid)
      (if (some? old-bufid)
        (->> ($hidden-input) $remove (.appendChild js/document.body)))
      (if (and (some? bufid)
               (-> bufid $cursor some?))
        (->> ($hidden-input) $remove (.appendChild ($cursor bufid)))))))

(add-listener
  :client-changed :ui-render
  (fn [[patch old-client client]]
    ;(println "client-changed:old:" old-client)
    ;(println "client-changed:new:" client)
    ;(println old-client)
    ;(println client)
    (render-views old-client client)
    (render-active-buffer old-client client)
    (doseq [bufid (-> patch :buffers keys)]
      ;(println "bufid:" bufid)
      (let [old-buf (-> old-client :buffers (get bufid))
            buf (-> client :buffers (get bufid))]
        (render-title old-client old-buf client buf)
        (render-buffer old-buf buf)
        (when (some? buf)
          (render-status-bar old-buf buf)
          (render-gutter old-buf buf)
          (render-lines old-buf buf)
          (render-visual old-buf buf)
          (render-highlights old-buf buf)
          (render-cursor old-buf buf)
          (render-ime old-buf buf)
          (render-scroll-top old-buf buf)
          (render-autocompl old-buf buf))))))
