(ns webvim.ui.view
  (:require [webvim.ui.lib.dom :refer [$id $hiccup $exist? add-class remove-class
                                       $remove $text-content beep $empty
                                       measure-text-size $show $hide line-height]]
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

(defn- render-editor []
  (if (not ($exist? "editor")) ;global ui
    (.appendChild js/document.body
                  ($hiccup [:div#editor
                            [:div#buffers]
                            [:div#status-bar.status-bar
                             [:span#status-bar-buf.ex]
                             [:span#status-bar-cursor.cursor]
                             [:span#status-bar-cursor-second.cursor.cursor-second]
                             [:span#status-bar-name.buf-name]
                             [:span#status-bar-keys.ongoing-keys]]
                            [:div#autocompl.autocompl]]))))

(defn- render-beep [old-client client]
  (if (client :beep?) (beep)))

(defn- render-title [old-client old-buf client buf]
  (if (or (diff? old-client client :cwd)
          (diff? old-buf buf :name))
    (set! js/document.title
          (str (buf :name)
               " - "
               (client :cwd)))))

(defn- render-buffer [{old-bufid :id} {bufid :id}]
  (println "render-buffer:" old-bufid bufid)
  (if (not= old-bufid bufid)
    (do
      (if (some? old-bufid)
        ($remove ($id (str "buffer-" old-bufid))))
      (let [domid (str "buffer-" bufid)]
        (if-not ($exist? domid)
          (.appendChild ($id "buffers")
                        ($hiccup [:div.buffer {:id domid}
                                  [:div.gutter {:id (str "gutter-" bufid)}]
                                  [:div.content {:id (str "content-" bufid)}
                                   [:div.lines {:id (str "lines-" bufid)}]
                                   [:div.cursor {:id (str "cursor-" bufid)}]
                                   [:div.cursor.cursor2 {:id (str "cursor2-" bufid)}]
                                   [:div.selections {:id (str "selections-" bufid)}]
                                   [:div.highlights {:id (str "highlights-" bufid)}]]])))))))

(defn- render-scroll-top [{old-scroll-top :scroll-top}
                          {scroll-top :scroll-top
                           bufid :id}]
  (if (not= old-scroll-top scroll-top)
    (set! (.-scrollTop ($id (str "buffer-" bufid)))
          (* scroll-top (line-height)))))

(add-listener
  :client-changed :ui-render
  (fn [[patch old-client client]]
    (let [old-buf (active-buf old-client)
          buf (active-buf client)]
      (render-editor)
      (render-beep old-client client)
      (render-title old-client old-buf client buf)
      (render-status-bar old-buf buf)
      (render-buffer old-buf buf)
      (render-gutter old-buf buf)
      (render-lines old-buf buf)
      (render-visual old-buf buf)
      (render-highlights old-buf buf)
      (render-cursor old-buf buf)
      (render-scroll-top old-buf buf)
      (render-autocompl old-buf buf))))
