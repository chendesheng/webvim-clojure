(ns webvim.keymap
  (:require [ring.adapter.jetty :as jetty]
            [clojure.core.async :as async]
            [ring.util.response :as response])
  (:use clojure.pprint
        webvim.buffer
        clojure.tools.trace
        webvim.autocompl))

(defonce motion-keymap (atom {}))
(defonce edit-keymap (atom {}))
(defonce normal-mode-keymap (atom {}))
(defonce visual-mode-keymap (atom {}))
(defonce insert-mode-keymap (atom {}))
(defonce ex-mode-keymap (atom {}))

;enter point of key sequence parser
(defonce root-keymap (atom {}))
(defonce registers (atom {}))

(defonce normal-mode 0)
(defonce insert-mode 1)
(defonce visual-mode 2)

(defonce key-server-in (async/chan))
(defonce key-server-out (async/chan))

(declare serve-keymap)
(declare map-fn-else)

(defn call-if-fn [b f & args]
  (if (fn? f)
    (apply f b args)
    b))

(defn testprint[obj prefix]
  (let[]
    (println prefix)
    (pprint obj)
    obj))

;two types: key (leaf), keymap (internal node)
;when visit a keymap call :enter :leave 
; keymap is repetitive if :continue return true
;when visit a key call :before :after
(defn serve-keys
  "Serve a sequence of keys until end of keymap. Recusivly walk through keymap tree (works like sytax parser)"
  [b in out keymap keycode]
  (let [f (keymap keycode)]
    (println "got key:" keycode)
    (if (or (fn? f) (map? f) (fn? (:else keymap)))
      (-> b
          (call-if-fn (:before keymap) keycode)
          (map-fn-else in out keymap keycode)
          (call-if-fn (:after keymap) keycode))
      b)))

(defn map-fn-else[b in out keymap keycode]
  (let [f (keymap keycode)]
    (cond
      (map? f)
      (serve-keymap b in out f keycode)
      (fn? f)
      (f b)
      (nil? f)
      (call-if-fn b (:else keymap) keycode))))

(defn loop-serve-keys[b in out keymap]
  (let [keycode (async/<!! in)
        b1 (serve-keys b in out keymap keycode)]
    (if (and (fn? (:continue keymap))
             ((:continue keymap) b1 keycode))
      (let[]
        (async/>!! out b1)
        (recur b1 in out keymap))
      b1)))

(defn send-out
  "write to out channel then return first argument"
  [obj out]
  (let[]
    (async/>!! out obj)
    obj))

(defn serve-keymap[b in out keymap keycode]
  (-> b
      (buffer-append-keys keycode)
      (call-if-fn (:enter keymap))
      (send-out out)
      (loop-serve-keys in out keymap)
      (call-if-fn (:leave keymap))))

(defn set-normal-mode[b]
  (print "set-normal-mode:")
  (-> b 
      (merge {:ex "" 
              :mode normal-mode 
              :keys nil 
              :autocompl {:suggestions nil 
                          :suggestions-index 0 
                          :words (-> b :autocompl :words)}})))

(defn set-visual-mode[b]
  (print "set-visual-mode:")
  (let [cur (-> b :cursor cursor-to-point)]
    (merge b {:ex "" :mode visual-mode :keys nil 
              :visual {:type 0 :ranges [cur cur]}})))

(defn set-insert-mode[b]
  (println "set-insert-mode")
  (buf-save-cursor (merge b {:ex "" :mode insert-mode :message nil :keys nil})))

(defn set-insert-append[b]
  (set-insert-mode
    (let [cursor (:cursor b)]
      (if (pos? (:col cursor))
        (let [newcol (-> cursor :col inc)]
          (assoc b :cursor (merge cursor {:col newcol :lastcol newcol})))
        (assoc b :cursor (merge cursor {:col 0 :lastcol 0}))))))

(defn set-ex-mode[b]
  (merge b {:ex ":" :message nil :keys nil}))

(defn set-ex-search-mode[b]
  (merge b {:ex "/" :message nil :keys nil}))

(defn insert-mode-default[b keycode]
  (println (str "insert-mode-default:" keycode))
  (let [b1 (cond 
             (= "backspace" keycode)
             (buf-delete b)
             (= "enter" keycode)
             (buf-insert b "\n")
             (= "space" keycode)
             (buf-insert b " ")
             (= "tab" keycode)
             (buf-insert b "\t") 
             (= 1 (count keycode))
             (buf-insert b keycode)
             :else
             b)]
    (if (empty? (-> b1 :autocompl :suggestions))
      b1
      (let [word (buffer-word-before-cursor b1)
            suggestions (-> b1 :autocompl :words (autocompl-suggest word))]
        (if (= 1 (count suggestions))
          (assoc-in b1 [:autocompl :suggestions] [])
          (assoc b1 :autocompl 
                 (merge (:autocompl b1) 
                        {:suggestions suggestions
                         :suggestions-index 0})))))))

(defn ex-mode-default[b keycode]
  (let [ex (:ex b)]
    (if (= (count keycode) 1)
      (assoc b :ex (str ex keycode))
      b)))

(defn execute [b]
  (let [ex (:ex b)]
    (cond
      (= ex ":w")
      (write-buffer b)
      (= ex ":e")
      (merge (open-file (:name b)) 
             {:cursor (:cursor b) 
              :message (str "\"" (:name b) "\" " (count (:lines b)) "L")})
      (= \/ (first ex))
      (let []
        (swap! registers assoc "/" (subs ex 1))
        (cursor-next-str b (@registers "/")))
      :else
      (assoc b :message "unknown command"))))

(defn save-lastbuf[b keycode]
  (-> b (assoc-in [:context :lastbuf] b)))

(defonce map-key-inclusive 
  {"h" false
   "l" false
   "w" false 
   "W" false 
   "e" true 
   "E" true 
   "b" false 
   "B" false 
   "f" true
   "F" false
   "t" true
   "T" false
   "/" false})

(defn inclusive? [keycode]
  (let [res (map-key-inclusive keycode)]
    (if (nil? res)
      true
      res)))

(defn delete-motion[b keycode]
  (println (str "delete-motion:" keycode))
  (println (str "inclusive:" (inclusive? keycode)))
  (-> b :context :lastbuf 
      buf-save-cursor
      (buf-replace 
        (:cursor b) "" (inclusive? keycode))
      history-save))

(defn change-motion[b keycode]
  (-> b :context :lastbuf
      (buf-replace (:cursor b) "" (inclusive? keycode))
      (serve-keymap key-server-in key-server-out (@normal-mode-keymap "i") keycode)))

(defn visual-mode-select[b keycode]
  (print "visual-mode-select:")
  (assoc-in b [:visual :ranges 1]
            (-> b :cursor cursor-to-point)))

(defn autocompl-start[b]
  (let [word (buffer-word-before-cursor b)
        suggestions (autocompl-suggest (-> b :autocompl :words) word)]
    (if (= 1 (count suggestions))
      (assoc-in b [:autocompl :suggestions] [])
      (assoc b :autocompl 
             (merge (:autocompl b) 
                    {:suggestions suggestions 
                     :suggestions-index 0})))))

(defn autocompl-move[b f]
  (let [b1 (if (empty? (-> b :autocompl :suggestions))
             (autocompl-start b)
             b)
        i (f (-> b1 :autocompl :suggestions-index))
        cnt (-> b1 :autocompl :suggestions count)]
    (if (zero? cnt)
      b1
      (let [n (mod (+ i cnt) cnt)]
        (-> b1 
            (assoc-in [:autocompl :suggestions-index] n)
            (buffer-replace-suggestion (-> b1 :autocompl :suggestions (get n))))))))

(defn move-to-next-char[b keycode]
  (let [ch (cond 
             (= 1 (count keycode))
             keycode
             (= "tab" keycode)
             "\t"
             (= "space" keycode)
             " ")]
    (cursor-next-char b ch)))

(defn move-to-back-char[b keycode]
  (let [ch (cond 
             (= 1 (count keycode))
             keycode
             (= "tab" keycode)
             "\t"
             (= "space" keycode)
             " ")]
    (cursor-back-char b ch)))

(defn delete-line[b]
  (-> b
    buf-save-cursor
    buf-delete-line
    history-save))

(defn init-keymap-tree
  []
  (reset! ex-mode-keymap
          {"enter" execute
           "space" #(assoc % :ex (str (:ex %) " "))
           "backspace" #(let [ex (subs (:ex %) 0 (-> % :ex count dec))]
                           (assoc % :ex ex))
           :continue #(not (or (= "esc" %2) (= "enter" %2) (empty? (:ex %1))))
           :else ex-mode-default})

  (reset! motion-keymap
          {"h" #(cursor-move-char % 0)
           "l" #(cursor-move-char % 1)
           "k" #(cursor-move-char % 2)
           "j" #(cursor-move-char % 3)
           "g" {"g" cursor-move-start}
           "G" cursor-move-end
           "w" cursor-next-word
           "W" cursor-next-WORD
           "b" cursor-back-word
           "B" cursor-back-WORD
           "e" cursor-word-end
           "E" cursor-WORD-end
           "0" cursor-line-first
           "^" cursor-line-start
           "$" cursor-line-end
           "f" { :else move-to-next-char }
           "F" { :else move-to-back-char }
           "t" { :else #(-> %1 
                            (move-to-next-char %2)
                            (cursor-move-char 0)) }
           "T" { :else #(-> %1
                            (move-to-back-char %2)
                            (cursor-move-char 1)) }
           "/" (merge @ex-mode-keymap 
                      {:enter set-ex-search-mode
                       :else ex-mode-default})
           "n" #(cursor-next-str % (@registers "/"))
           "N" #(cursor-back-str % (@registers "/"))
           "c+u" #(cursor-move-viewport %1 -0.5) 
           "c+d" #(cursor-move-viewport %1 0.5)})

  (reset! visual-mode-keymap @motion-keymap)
  (swap! visual-mode-keymap 
         merge
          {"z" {"z" cursor-center-viewport}})

  (reset! insert-mode-keymap 
          {;"c+o" @normal-mode-keymap 
           "c+n" #(autocompl-move % inc)
           "c+p" #(autocompl-move % dec)
           :else insert-mode-default 
           :enter set-insert-mode
           :continue #(not (= "esc" %2))
           :leave #(-> %
                       (cursor-move-char 0)
                       set-normal-mode
                       history-save)})

  (reset! normal-mode-keymap @motion-keymap)
  (swap! normal-mode-keymap 
         merge 
         {"i" @insert-mode-keymap
          "a" (merge
                @insert-mode-keymap
                {:enter set-insert-append})
          ":" (merge
                @ex-mode-keymap
                {:enter set-ex-mode
                 :leave set-normal-mode})
          "u" history-undo
          "c+r" history-redo
          "esc" set-normal-mode
          "v" (merge
                @visual-mode-keymap
                {:enter set-visual-mode
                 :leave set-normal-mode
                 :continue #(not (or (= "esc" %2) (= "v" %2)))
                 :after visual-mode-select})
          "z" {"z" cursor-center-viewport }
          "d" (merge 
                @motion-keymap
                {:before save-lastbuf 
                 :after delete-motion
                 ;"j" delete-line
                 ;"k" delete-line
                 "d" delete-line})
          "D" delete-line
          "c" (merge
                @motion-keymap
                {:before save-lastbuf 
                 :after change-motion })})
  (reset! root-keymap @normal-mode-keymap))
