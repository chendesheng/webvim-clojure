(ns webvim.core
  (:require [ring.adapter.jetty :as jetty]
            [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [ring.util.response :as response])
  (:use clojure.pprint
        (clojure [string :only (join split)])
        (compojure handler [core :only (GET POST defroutes)])
        (hiccup [page :only (html5)])
        ring.middleware.resource
        ring.util.response
        ring.middleware.json))

(defn split-lines-all [txt]
  (split txt #"\r?\n" -1))
(defn hello [x]
  (println "hello"))
(defn open-file[f]
  (let [txt (slurp f)]
  {:lines (split-lines-all txt)
   :cursor [0 0 0 0] 
   ;:cursor {:row 0 :col 0 :vlastcol 0 :vcol 0}
   :sel-range []
   :mode 0
   :name f
   :ex ""
   :screen {:w 0 :h 0}}))

(def test-buf (open-file "/users/roy/webvim/testfile.clj"))

(defn row-count [b row]
  (count ((:lines b) row)))

(defn replace-range [lines [[r1 c1] [r2 c2]] txt]
  "insert delete replace in buffer"
  (let [prefix (subvec lines 0 r1)
        suffix (subvec lines (inc r2))
        l1 (lines r1)
        l2 (lines r2)
        txt-lines (split-lines-all txt)
        newcol (if (= 1 (count txt-lines))
                 (+ c1 (count (first txt-lines)))
                 (count (last txt-lines)))
        middle (update
                    (update txt-lines 0 #(str (subs l1 0 c1) %))
                    (dec (count txt-lines))
                    #(str % (subs l2 c2)))]
      {:lines (vec (concat prefix middle suffix))
     :cursor [(+ r1 (dec (count txt-lines))), newcol, newcol]}))


(defn buf-insert [b txt]
  (merge b (replace-range 
             (:lines b) 
             [(:cursor b) (:cursor b)]
             txt))) 

(defn buf-delete [b]
  (let [[row col lastcol] (:cursor b)] 
    (if (= 0 row col)
      b
      (if (= col 0)
        (merge b (replace-range 
                   (:lines b) 
                   [[(dec row) (count ((:lines b) (dec row)))] (:cursor b)] ""))
        (merge b (replace-range
                   (:lines b)
                   [[row (dec col)] (:cursor b)] ""))))))


(defn calc-col [b row col lastcol]
  (let [cnt (row-count b row)
        tmp (cond 
              (> cnt lastcol) lastcol
              (< cnt 1) 0
              :else (dec cnt))]
    [row tmp lastcol]))

;(calc-col test-buf 4 30 30)

(defn cursor-move-end[b]
  (assoc b :cursor [(dec (count (:lines b))) 0 0]))


(defn cursor-move-char [b direction]
  (let [[row col lastcol] (:cursor b)]
    (assoc b :cursor 
           (cond 
             ;move left
             (and (= direction 0) (pos? col))
             (let [c (dec col)]
               [row c c])

             ;move right
             (and (= direction 1) (> (row-count b row) (inc col)))
             (let [c (inc col)]
               [row c c])

             ;move up
             (and (= direction 2) (pos? row))
             (calc-col b (dec row) col lastcol)

             ;move down
             (and (= direction 3) (> (count (:lines b)) (inc row)))
             (calc-col b (inc row) col lastcol)

             :else (:cursor b)))))

(defn re-pos [re s]
  (loop [m (re-matcher re s)
         res {}]
    (if (.find m)
      (recur m (assoc res (.start m) (.group m)))
      res)))
 
;(first-nonspace-pos "   aaa")
(defn first-nonspace-pos[line]
  (let [m (re-matcher #"\S" line)]
    (if (.find m)
      (.start m)
      0)))

(defn round-to-zero[i]
  (println "i:" i)
  (if (> i 0)
    (int i)
    (- (int (- i)))))

;(round-to-zero -9.1)
;(round-to-zero 9.1)

(defn cursor-move-screen [b factor]
  (let [d (round-to-zero (* (:h (:screen b)) factor))
        row (+ ((:cursor b) 0) d)
        aa (println "d:" (round-to-zero d))
        bb (println "row:" row)
        newrow (cond 
                 (< row 0)
                 0

                 (>= row (-> b :lines count))
                 (-> b :lines count dec)

                 :else
                 row)
        newcol (first-nonspace-pos ((:lines b) newrow))]
    (assoc b :cursor [newrow newcol newcol])))



(defn set-normal-mode[b]
  (assoc (cursor-move-char b 0) :mode 0))

(defn insert-mode-default[b code]
  (cond 
    (= "enter" code)
    (buf-insert b "\n")
    (= "space" code)
    (buf-insert b " ")
    (= "tab" code)
    (buf-insert b "\t")
    :else
    (if (= 1 (count code)) 
      (buf-insert b code)
      b)))

;global data
(defonce active-buffer (atom (open-file "/users/roy/webvim/testfile.clj")))

;(reset! active-buffer test-buf)

(defonce buffer-list (atom {}))
(defonce jump-list (atom ()))
(defonce registers (atom {}))
(def normal-keymap 
  {"h" #(cursor-move-char % 0)
   "l" #(cursor-move-char % 1)
   "k" #(cursor-move-char % 2)
   "j" #(cursor-move-char % 3)
   "G" cursor-move-end
   "c+u" #(cursor-move-screen %1 -0.5) 
   "c+d" #(cursor-move-screen %1 0.5)
   "i" #(merge % {:mode 1 :message nil})
   ":" #(merge % {:mode 2 :ex ":" :message nil})})

(def insert-keymap
  {"esc" set-normal-mode
   "c+d" #(-> % (cursor-move-char 1) buf-delete)
   "backspace" buf-delete})


;(defn count-lines [b]
;  (count (:lines b)))

;(count-lines test-buf)
;(= (join "\n" (:lines test-buf)) (:text test-buf))

;(defn insert-single-line [line s txt]
;  (let 
;    [prefix (subs line 0 s)
;     suffix (subs line s)
;     lines (split-lines-all txt)]
;    (update 
;      (update 
;        lines 0 #(str prefix %))
;      (dec (count lines))
;      #(str % suffix))))
;
;(insert-single-line "hello" 2 "yes\nyes")

;(replace-range ["hello" "ok" "yes"] [[0 0] [1 1]] "insert")
;(replace-range ["hello" "ok" "yes"] [[1 1] [1 1]] "in\nsert")
;(replace-range ["hello" "ok" "yes"] [[2 0] [2 3]] "in\nsert")
;(def test-buf (cursor-move-char
; (cursor-move-char test-buf 2) 2))
;
;(def test-buf (assoc test-buf :cursor [5 30 30]))
;(:cursor (cursor-move-char test-buf 2))
(def test-buf (buf-insert test-buf "\n"))
(count (:lines test-buf))
;(def test-buf (replace-range (:lines test-buf) (:cursor test-buf) "a"))

(defn homepage
  [request]
  (html5 
    [:head
     [:script {:src "http://libs.baidu.com/jquery/2.0.3/jquery.js" :type "text/javascript"}]
     [:script {:src "main.js" :type "text/javascript"}]
     [:link {:href "main.css" :rel "stylesheet"}]]
    [:body
     [:div.gutter]
     [:div.lines]
     [:div.status-bar [:pre]]]))

(defn render [buf]
  (response (dissoc buf :screen :name)))

(defn write-buffer [b]
  (try 
    (if (fs/writeable? (:name b))
      (let [lines (:lines b)
            f (:name b)
            tmp (str (fs/tmpdir) (fs/base-name f))]
        (if (fs/exists? tmp)
          (fs/delete tmp))
        (fs/create (fs/file tmp))
        (with-open [wrtr (io/writer (fs/file tmp))]
          (doseq [line lines]
            (do (.write wrtr line)
                (.write wrtr "\n"))))
        ;TODO handle line break
        ;TODO fsync before rename
        (merge b {:ex "" :mode 0 :message (str "\"" f "\" " (count lines) "L written")})))
    (catch Exception e 
      (let [err (str "caught exception: " (.getMessage e))]
        (merge b {:ex "" :mode 0 :message err})))))

(defn execute [b]
  (let [ex (:ex b)]
    (cond
      (= ex ":w")
      (write-buffer b)
      (= ex ":e")
      (merge (open-file (:name b)) 
             {:cursor (:cursor b) 
              :message (str "\"" (:name b) "\" " (count (:lines b)) "L")})
      :else
      (merge b {:ex "" :message "unknown command" :mode 0}))))

(defn edit-ex-mode[b keycode]
  (cond 
    (= keycode "enter")
    (execute b)
    (= keycode "esc")
    (merge b {:ex "" :mode 0 :message nil})
    :else
    (let [ex (:ex b)]
      (cond 
        (= (count keycode) 1)
        (assoc b :ex (str ex keycode))
        (= keycode "space")
        (assoc b :ex (str ex " "))
        (= keycode "backspace")
        (let [ex2 (subs ex 0 (dec (count ex)))]
          (if (empty? ex2)
            (merge b {:mode 0 :ex ""})
            (assoc b :ex ex2)))
        :else
        b))))

(defn edit-normal-mode[b keycode]
  (let [f (normal-keymap keycode)]
    (if (nil? f)
      b
      (f b))))

(defn edit-insert-mode[b keycode]
  (let [f (insert-keymap keycode)]
    (if (nil? f)
      (insert-mode-default b keycode)
      (f b))))

(defn do-edit[b keycode] 
  (let [mode (:mode b)]
    (cond 
      (== 0 mode)
      (edit-normal-mode b keycode)
      (== 1 mode)
      (edit-insert-mode b keycode)
      (== 2 mode)
      (edit-ex-mode b keycode))))

(defn edit [keycode]
  (let [before @active-buffer
        after (swap! active-buffer do-edit keycode)]
    (if (not (= before after))
      (if (not (= (:lines before) (:lines after)))
        (render after)
        (render (dissoc after :lines)))
      "")))

(defn parse-int [s]
  (Integer. (re-find #"\d+" s)))

(defroutes main-routes
  (GET "/" [request] (homepage request))
  (GET "/buf" [] (response @active-buffer))
  (GET "/resize/:w/:h" [w h] 
       (swap! active-buffer assoc :screen {:w (parse-int w) :h (parse-int h)}))
  (GET "/key/:keycode" [keycode] (edit keycode)))

(def app
  ;(wrap-json-response main-routes))
  (-> (compojure.handler/site main-routes)
      (wrap-json-response)
      (wrap-resource "public")))

;(jetty/run-jetty #'app {:port 8080 :join? false})









