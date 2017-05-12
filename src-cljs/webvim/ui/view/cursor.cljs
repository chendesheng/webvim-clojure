(ns webvim.ui.view.cursor
  (:require
    [clojure.string :as string]
    [webvim.ui.lib.dom :refer [bounding-rect $id $text-content
                               toggle-class $hiccup rect-pos
                               line-height $show $hide $toggle
                               get-element-and-offset
                               add-class remove-class $linenum]]))

(defn- cursor-position [$lines-nodes px py]
  (let [$cur-line (aget $lines-nodes py)]
    (if (some? $cur-line)
      (let [rect (apply bounding-rect
                        (get-element-and-offset $cur-line px))
            [x y] (rect-pos rect)
            h (.-height rect)]
        ;(println "xy:" x y)
        [x (dec y) h]))))

(defn- cursor-position-in-buffer [$lines cx cy]
  (let [[linesx linesy :as lines-pos] (rect-pos (bounding-rect $lines))
        [px py h] (or (cursor-position (.-childNodes $lines) cx cy) lines-pos)]
    [(- px linesx) (- py linesy) h]))

;TODO: fix reverse-background?
(defn- render-cursor-inner [$lines $cur cx cy reverse-background?]
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
        (-> .-style .-background (set! "#fff"))
        (-> .-style .-width (set! "1ch"))
        (-> .-style .-height (set! (str (line-height) "px")))))
    (doto (.-style $cur)
      (-> .-left (set! (str px "px")))
      (-> .-top (set! (str py "px"))))))

(defn- set-scroll-top [bufid scroll-top]
  (set! (.-scrollTop ($id (str "buffer-" bufid)))
        (* scroll-top (line-height))))

(defn- set-scroll-left [bufid $cursor]
  (let [$content ($id (str "content-" bufid))
        scroll-left (.-scrollLeft $content)
        offset-width (.-offsetWidth $content)
        scroll-right (+ scroll-left offset-width)
        cur-left (.-offsetLeft $cursor)
        cur-right (+ (.-offsetLeft $cursor) (.-offsetWidth $cursor))
        new-scroll-left (cond
                          (< cur-left scroll-left)
                          (if (< cur-left offset-width) 0 cur-left)
                          (> cur-right scroll-right)
                          (- cur-right offset-width))]
    (println "new-scroll-left=" new-scroll-left)
    (if (some? new-scroll-left)
      (set! (.-scrollLeft $content) new-scroll-left))))

(defn render-cursor [{old-bufid :id
                      [x01 y01 :as old-cursor] :cursor
                      [x02 y02 :as old-cursor2] :cursor2}
                     {bufid :id
                      scroll-top :scroll-top
                      [x11 y11 :as cursor] :cursor
                      [x12 y12 :as cursor2] :cursor2}]
  (let [$lines ($id (str "lines-" bufid))]
    (if (and (some? cursor)
             (or (not= old-cursor cursor)
                 (not= old-bufid bufid)))
      (let [$cursor ($id (str "cursor-" bufid))]
        (render-cursor-inner
          $lines
          $cursor
          x11 y11
          false)
        (when (not= y01 y11)
          (if y01 (remove-class ($linenum old-bufid y01) "highlight"))
          (add-class ($linenum bufid y11) "highlight"))
        (set-scroll-top bufid scroll-top)
        (set-scroll-left bufid $cursor)))
    (if (or (not= old-bufid bufid)
            (not= old-cursor2 cursor2))
      (let [$cursor2 ($id (str "cursor2-" bufid))]
        ($toggle $cursor2 (not (empty? cursor2)))
        (if-not (empty? cursor2)
          (render-cursor-inner
            $lines
            $cursor2
            x12 y12
            false))))))
