(ns webvim.main
  (:require [clojure.core.async :as async]
            [ring.util.response :as response]
            [cheshire.core :as json]
            [ring.adapter.jetty9 :as jetty])
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
        webvim.render
        (compojure handler [core :only (GET POST defroutes)])
        (hiccup [page :only (html5)])
        ring.middleware.resource
        ring.util.response
        ring.middleware.json))

(defn- parse-int [s]
  (Integer. (re-find #"\d+" s)))

(defn- homepage
  [request]
  (html5 
    [:head
     [:script {:src "jquery.js" :type "text/javascript"}]
     [:script {:src "socket.js" :type "text/javascript"}]
     [:script {:src "utils.js" :type "text/javascript"}]
     [:script {:src "dom.js" :type "text/javascript"}]
     [:script {:src "keycode.js" :type "text/javascript"}]
     [:script {:src "keymap.js" :type "text/javascript"}]
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

(defn- active-buffer[]
  (let [b (registers-get registers "%")]
    (if (nil? b) 
      nil
      (@buffer-list (b :id)))))

(defroutes main-routes
  (GET "/" [request] (homepage request))
  (GET "/buf" [] (response [(render nil (or @(active-buffer)
                                            @(second 
                                               (first @buffer-list))))]))
  (GET "/resize/:w/:h" [w h] 
       (swap! window update-in [:viewport] merge {:w (parse-int w) :h (parse-int h)})))

(def ^:private app
  (-> (compojure.handler/api main-routes)
      (wrap-json-response)
      (wrap-resource "public")))

(defn- start-file[f]
  (let [buf @(new-file f)]
    (registers-put! registers "%" {:str f :id (buf :id)})))

(defn- parse-input[body]
  (let [[id keycode] 
        (-> #"(?s)(\d+)\!(.*)"
            (re-seq body)
            first
            rest)]
    [(Integer. id) keycode]))

;(parse-input "123!!\n")
(defonce ^:private ws-out (atom nil))

(defn- read-output![buffers]
  (let [[buf _] (async/alts!! (vec (map 
                      (fn[[_ buf]] (buf :chan-out))
                      buffers)))]
    (if (nil? buf)
      [nil nil]
      [(dissoc buf :nextid) (buf :nextid)])))


(defn- change-func[keycode]
  (fn[buf]
    (webvim.core.rope/buf-insert buf (str keycode "\n"))))

(defn- change-buffer![buf keycodes ws]
  (println keycodes)
  (let [[newbuf changes] (reduce 
                           (fn[[buf changes] keycode]
                             (let [newbuf ((change-func keycode) buf)
                                   newchanges (newbuf :changes)]
                               [(assoc newbuf :changes []) 
                                (concat changes newchanges)])) [buf nil] keycodes)
        diff (render buf (assoc newbuf :changes changes))]
    (println diff)
    (jetty/send! ws (json/generate-string diff))
    newbuf))

;try to send changes to client
;if exception happens return rest of chs
;else return emtpy vector
;almost always contains only one change
(defn- flush-changes! [ws changes]
  (loop [chs changes]
    (if (empty? chs) 
      []
      (let [success? (try
                       (do
                         (let [s (json/generate-string (first chs))]
                           (time (jetty/send! ws s)))
                         true)
                       (catch Exception e
                         (println e)
                         (reset! ws-out nil)
                         false))]
        (if success?
          (recur (rest chs))
          chs)))))

(defn- listen-output[]
  (async/thread
    (loop[changes []]
      (let [[newbuf nextid] (read-output! @buffer-list)]
        (if (nil? newbuf) 
          (recur changes)
          (let [id (newbuf :id)
                buf (@buffer-list id)
                nextbuf (@buffer-list nextid)
                ws @ws-out
                chs (flush-changes! ws (conj changes (render buf newbuf)))]
            (save-buffer! newbuf)
            (if (or (nil? nextid) (= id nextid))
              (recur chs)
              (recur (flush-changes! ws 
                                     (conj changes (render nil nextbuf)))))))))))

(defn- handle-socket[req]
  {:on-connect (fn[ws]
                 (reset! ws-out ws))
   :on-text (fn[ws body]
              (let [[id keycode] (parse-input body)]
                ;It's ok to use send here because this server only serve one client.
                (send (@buffer-list id) change-buffer! (input-keys keycode) ws)))})

;start app with init file and webserver configs
(defn start[file options]
  (init-keymap-tree)
  (start-file file)
  (jetty/run-jetty #'app 
                   (assoc options :websockets {"/socket" handle-socket})))

(defn -main[& args]
  (start 
    "/tmp/webvim/welcome.txt" 
    {:port 8080 :join? true}))
