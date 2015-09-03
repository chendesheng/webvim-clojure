(ns webvim.core
  (:require [me.raynes.fs :as fs]
            [ring.adapter.jetty :as jetty]
            [clojure.core.async :as async]
            [ring.util.response :as response])
  (:use clojure.pprint
        webvim.buffer
        webvim.keymap
        webvim.global
        webvim.serve
        webvim.autocompl
        (compojure handler [core :only (GET POST defroutes)])
        (hiccup [page :only (html5)])
        ring.middleware.resource
        ring.util.response
        ring.middleware.json))

(defn dissoc-empty[b ks]
  (if (empty? (get-in b ks))
    (if (= 1 (count ks))
      (dissoc b (first ks))
      (recur (update-in b (pop ks) dissoc (peek ks)) (pop ks)))
    b))

(defn dissoc-nil[b k]
  (if (nil? (b k))
    (dissoc b k)
    b))

(defn diff-lines [after before]
  (let [lines1 (:lines before)
        lines2 (:lines after)]
    (cond 
      (= lines1 lines2)
      (dissoc after :lines)
      :else
      ;assume we only change single continuous lines block
      (let [c1 (count lines1)
            c2 (count lines2)
            ;_ (println "c1:" c1 " c2:" c2)
            start (loop [i 0];top-down
                    (if (and (> c1 i) (> c2 i) (= (lines1 i) (lines2 i)))
                      (recur (inc i))
                      i))
            end (loop [i 1];bottom-up, end is inclusive
                  (let [i1 (- c1 i)
                        i2 (- c2 i)]
                    ;(println "(<= start i1)" (<= start i1))
                    ;(println "(<= start i2)" (<= start i2))
                    ;(println "(lines1 i1) == (lines2 i2):" (= (lines1 i1) (lines2 i2)))
                    (if (and (< 0 i1) (< 0 i2) (<= start i1) (<= start i2) (= (lines1 i1) (lines2 i2)))
                      (recur (inc i))
                        i2)))
            ;_ (println "start:" start " end:" end)
            add-lines (subvec lines2 start (inc end))
            ;c1+count(add-lines)-count(sub-lines)=c2
            sub-lines (-> c1 (+ (count add-lines)) (- c2))]
        (if (pos? sub-lines)
          (autocompl-words-remove-lines (subvec lines1 start (+ start sub-lines))))
        (autocompl-words-parse-lines add-lines)
        (-> after
            (dissoc :lines)
            (assoc :difflines {:row start :sub sub-lines :add add-lines}))))))

(defn- track-unsaved[b]
  (if (buf-lines-unsaved? b)
    (assoc b :unsaved 1)
    (assoc b :unsaved 0)))

(defn- remove-visual-mode[b]
  (if (empty? (-> b :visual :ranges))
    (dissoc b :visual)
    b))

(defn- remove-autocompl[b]
  (if (empty? (-> b :autocompl :suggestions))
    (dissoc b :autocompl)
    b))

(defn- equal-cursor?[c1 c2]
  (if (= c1 c2)
    true
    (and (= (:row c1) (:row c2))
         (= (:col c1) (:col c2))
         (= (:lastcol c1) (:lastcol c2)))))

(defn- dissoc-cursor[after before]
  (let [braces (vec (filter ;remove brace if equal to :cursor
                            #(not (equal-pt? % (after :cursor))) 
                            (after :braces)))
        b (if (empty? braces)
            after
            (assoc after :braces braces))]
    ;dissoc :cursor if not change
    (if (equal-cursor? (:cursor before) (:cursor b))
      (dissoc b :cursor)
      b)))

(defn- remove-fields[b]
  (-> b 
      track-unsaved
      (dissoc :history :txt-cache :context :last-cursor :language :filepath :x :vx :y
          :macro :chan-in :chan-out :registers :last-saved-lines)
      (dissoc-empty [:highlights])
      (dissoc-empty [:changes])
      (dissoc-nil :keys)
      remove-visual-mode
      remove-autocompl))

(defn- dissoc-if-equal[after before k]
  (if (= (before k) (after k))
    (dissoc after k)
    after))

(defn render 
  "Write changes to browser."
  [before after]
  (let [txt (after :str)
        b (cond (= before after)
                ""
                (or (nil? before) (not (= (:id before) (:id after))))
                (-> after
                    (assoc :str (str txt))
                    (dissoc-cursor nil)
                    (assoc :lang (-> after :language :name))
                    (dissoc :changes)
                    remove-fields)
                :else
                (-> after
                    remove-fields
                    (dissoc :str)
                    (diff-lines before)
                    (dissoc-cursor before)
                    (dissoc-if-equal before :mode)
                    (dissoc-if-equal before :braces)
                    (dissoc-if-equal before :keys)
                    (dissoc-if-equal before :name)
                    (dissoc-if-equal before :ex)
                    (dissoc-if-equal before :message)
                    (dissoc-if-equal before :pos)
                    (dissoc :lines)))]
    (response b)))


(defn restart-key-server
  "For repl"
  []
  (init-keymap-tree)
  (let [b (active-buffer)
        _ (async/close! (:chan-in b))
        b2 (-> b
               (assoc :chan-in (async/chan))
               (assoc :chan-out (async/chan)))]
    (swap! buffer-list assoc (:id b2) b2)
    (key-server b2 @root-keymap)
    nil))

;TODO How to run code only in repl env? Conditional compile?
;only for testing on repl
(defonce open-test-file
  (swap! registers 
         assoc "%" 
         (reset! active-buffer-id 
                 (-> "testfile.clj"
                     open-file
                     buffer-list-save
                     :id))))

(defn update-buffer [b]
  ;not contains? means already deleted
  (swap! buffer-list 
         #(if (contains? % (:id b))
            (assoc % (:id b) b) %)))
  
(restart-key-server)
(defn edit [keycode]
  (let [before (active-buffer)]
    (async/>!! (:chan-in before) keycode)
    (let [after (async/<!! (:chan-out before))]
      (update-buffer 
        (if (= (before :lines) (after :lines))
          after (dissoc after :highlights)))

      ;Always write (active-buffer) back because active-buffer-id may change by current key
      (render before (active-buffer)))))

(defn parse-int [s]
  (Integer. (re-find #"\d+" s)))

;I don't like include js library directly, but also don't want download it again and again.
(defonce cache-jquery
  (let [path "resources/public/jquery.js"]
    (or 
      (fs/exists? path)
      (spit path (slurp "http://libs.baidu.com/jquery/2.0.3/jquery.js")))))

(defn homepage
  [request]
  (html5 
    [:head
     [:script {:src "jquery.js" :type "text/javascript"}]
     [:script {:src "keycode.js" :type "text/javascript"}]
     [:script {:src "keyboard.js" :type "text/javascript"}]
     [:script {:src "syntax/clojure.js" :type "text/javascript"}]
     [:script {:src "syntax/css.js" :type "text/javascript"}]
     [:script {:src "syntax/xml.js" :type "text/javascript"}]
     [:script {:src "syntax/javascript.js" :type "text/javascript"}]
     [:script {:src "highlight.js" :type "text/javascript"}]
     [:script {:src "main.js" :type "text/javascript"}]
     [:link {:href "main.css" :rel "stylesheet"}]
     [:link {:href "monokai.css" :rel "stylesheet"}]]
    [:body
     [:div.buffer]
     [:div.status-bar [:span.ex] [:span.ongoing-keys] [:span.buf-name]]]))

(defroutes main-routes
  (GET "/" [request] (homepage request))
  (GET "/buf" [] (render nil (active-buffer)))
  (GET "/resize/:w/:h" [w h] 
       (swap! window update-in [:viewport] merge {:w (parse-int w) :h (parse-int h)}))
  (GET "/key" {{keycode :code} :params} (time (edit keycode))))


(def app
  ;(wrap-json-response main-routes))
  (-> (compojure.handler/api main-routes)
      (wrap-json-response)
      (wrap-resource "public")))

(defonce run-jetty (jetty/run-jetty #'app {:port 8080 :join? false}))
