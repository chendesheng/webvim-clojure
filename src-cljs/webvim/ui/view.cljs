(ns webvim.ui.view
  (:require [webvim.ui.lib.dom :refer [$id $hiccup $exist? add-class remove-class
                                       $remove $text-content beep $empty $hidden-input
                                       $status-bar-cursor $buffer $view $exist?
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
  (not= (o1 key) (o2 key)))

(defn- active-buf [{active :active-buf buffers :buffers}]
  (if (some? buffers)
    (buffers active)))

(defn- render-beep [old-buf buf]
  (if (buf :beep?) (beep)))

(defn- render-title [old-client old-buf client buf]
  (if (or (diff? old-client client :cwd)
          (diff? old-buf buf :name))
    (set! js/document.title
          (str (buf :name)
               " - "
               (client :cwd)))))

(defn- render-buffer [{old-bufid :id old-view :view} {bufid :id view :view}]
  (if (not= old-bufid bufid)
    (let [$input ($hidden-input)]
      ($remove $input)
      (if (and
            (not= old-bufid bufid)
            (= old-view view)
            (some? old-bufid))
        ($remove ($id (str "buffer-" old-bufid))))

      (if (-> (str "buffer-" bufid) $exist? not)
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
                           [:span.ongoing-keys {:id (str "status-bar-keys-" bufid)}]]))))
      (-> bufid $cursor (.appendChild $input))
      (when (not= old-view view)
        (remove-class ($view old-view) "view-active")
        (add-class ($view view) "view-active")))))

(defn- render-scroll-top [{old-scroll-top :scroll-top}
                          {scroll-top :scroll-top bufid :id}]
  (if (not= old-scroll-top scroll-top)
    (set! (.-scrollTop ($id (str "buffer-" bufid)))
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
          [:div.view-box.view-box-hor style
           (render-views-recur viewa :height size)
           (render-views-recur viewb :height (- 1 size))]
          (= type "|")
          [:div.view-box.view-box-ver style
           (render-views-recur viewa :width size)
           (render-views-recur viewb :width (- 1 size))]))
      [:div.view (assoc style :id (str "view-" view))])))

(defn- render-views [{old-views :views} {views :views buffers :buffers}]
  (if (not= old-views views)
    (let [$newviews ($hiccup (render-views-recur views :width 1.0))
          tmpdoc (js/document.createDocumentFragment)]
      (.appendChild tmpdoc $newviews)
      (doseq [{bufid :id view :view} buffers]
        (if-let [$buf ($buffer bufid)]
          (-> tmpdoc
              (.getElementById (str "view-" view))
              (.appendChild (-> bufid $buffer $remove)))))
      (doto ($id "buffers")
        ($empty)
        (.appendChild (.-firstChild tmpdoc)))
      (doseq [buf buffers] ;; FIXME: we lost scroll left here
        (render-scroll-top nil buf)))))

(add-listener
  :client-changed :ui-render
  (fn [[patch old-client client]]
    (render-views old-client client)
    (let [old-buf (active-buf old-client)
          buf (active-buf client)]
      (render-beep old-buf buf)
      (render-title old-client old-buf client buf)
      (render-buffer old-buf buf)
      (render-status-bar old-buf buf)
      (render-gutter old-buf buf)
      (render-lines old-buf buf)
      (render-visual old-buf buf)
      (render-highlights old-buf buf)
      (render-cursor old-buf buf)
      (render-ime old-buf buf)
      (render-scroll-top old-buf buf)
      (render-autocompl old-buf buf))))
