(ns webvim.server
  (:require [me.raynes.fs :as fs]
            [ring.adapter.jetty :as jetty]
            [clojure.core.async :as async]
            [ring.util.response :as response])
  (:use clojure.pprint
        webvim.lang.default
        webvim.lang.clojure ;TODO: load language setting dynamically
        webvim.lang.javascript
        webvim.lang.css
        webvim.core.buffer
        webvim.core.serve
        webvim.core.register
        webvim.core.keys
        webvim.keymap
        webvim.keymap.action
        (compojure handler [core :only (GET POST defroutes)])
        (hiccup [page :only (html5)])
        ring.middleware.resource
        ring.util.response
        ring.middleware.json))

(defn dissoc-empty[buf ks]
  (if (empty? (get-in buf ks))
    (if (= 1 (count ks))
      (dissoc buf (first ks))
      (recur (update-in buf (pop ks) dissoc (peek ks)) (pop ks)))
    buf))

(defn dissoc-nil[buf k]
  (if (nil? (buf k))
    (dissoc buf k)
    buf))

(defn- remove-visual-mode[buf]
  (if (empty? (-> buf :visual :ranges))
    (dissoc buf :visual)
    buf))

(defn- remove-autocompl[buf]
  (if (empty? (-> buf :autocompl :suggestions))
    (dissoc buf :autocompl)
    buf))

(defn- equal-cursor?[c1 c2]
  (if (= c1 c2)
    true
    (and (= (:row c1) (:row c2))
         (= (:col c1) (:col c2))
         (= (:lastcol c1) (:lastcol c2)))))

(defn- remove-fields[buf]
  (-> buf 
      (dissoc :history :context :last-cursor :language :filepath :x :y :cursor :pending-undo :before-send-out :after-send-out :macro :chan-in :chan-out :registers :linescnt :root-keymap :ext)
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
        buf (cond (= before after)
                ""
                (or (nil? before) (not (= (:id before) (:id after))))
                (-> after
                    (assoc :str (str txt))
                    (assoc :lang (-> after :language :name))
                    (dissoc :changes)
                    remove-fields)
                :else
                (-> after
                    remove-fields
                    (dissoc :str)
                    (dissoc-if-equal before :mode)
                    (dissoc-if-equal before :keys)
                    (dissoc-if-equal before :name)
                    (dissoc-if-equal before :ex)
                    (dissoc-if-equal before :dirty)
                    (dissoc-if-equal before :message)
                    (dissoc-if-equal before :pos)))]
    buf))

(init-keymap-tree)

;TODO How to run code only in repl env? Conditional compile?
;only for testing on repl
(defonce open-test-file
  (registers-put
         registers
         "%" 
         (reset! active-buffer-id 
                 (-> "testfile.clj"
                     new-file
                     :id))))

(defn restart-key-server
  "For repl"
  []
  (let [buf (active-buffer)
        _ (async/close! (:chan-in buf))
        b2 (-> buf
               (assoc :chan-in (async/chan))
               (assoc :chan-out (async/chan)))]
    (swap! buffer-list assoc (:id b2) b2)
    (key-server b2)
    nil))

(defn update-buffer [buf]
  ;not contains? means already deleted
  (swap! buffer-list 
         #(if (contains? % (:id buf))
            (assoc % (:id buf) buf) %)))

(restart-key-server)

(defn edit [keycode]
  (let [before (active-buffer)]
    (async/>!! (:chan-in before) keycode)
    (let [after (async/<!! (:chan-out before))]
      (update-buffer after)
      ;Always write (active-buffer) back because active-buffer-id may change by current key
      (render before (active-buffer)))))

(defn handle-keys
  [s]
  (println s)
  (response 
    (reduce
      (fn [changes keycode]
        (conj changes (edit keycode))) [] (input-keys s))))

(defn parse-int [s]
  (Integer. (re-find #"\d+" s)))

;I don't like include js library directly, but also don't want download it again and again.
(defonce cache-jquery
  (let [path "resources/public/jquery.js"]
    (if-not (fs/exists? path)
      (spit path (slurp "http://libs.baidu.com/jquery/2.0.3/jquery.js")))))

(defn homepage
  [request]
  (html5 
    [:head
     [:script {:src "jquery.js" :type "text/javascript"}]
     [:script {:src "utils.js" :type "text/javascript"}]
     [:script {:src "dom.js" :type "text/javascript"}]
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
    [:body]))

(defroutes main-routes
  (GET "/" [request] (homepage request))
  (GET "/buf" [] (response [(render nil (active-buffer))]))
  (GET "/resize/:w/:h" [w h] 
       (swap! window update-in [:viewport] merge {:w (parse-int w) :h (parse-int h)}))
  (GET "/key" {{keycode :code} :params} (time (handle-keys keycode))))


(def app
  ;(wrap-json-response main-routes))
  (-> (compojure.handler/api main-routes)
      (wrap-json-response)
      (wrap-resource "public")))

(defonce run-jetty (jetty/run-jetty #'app {:port 8080 :join? false}))


