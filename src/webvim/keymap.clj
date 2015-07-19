(ns webvim.keymap
  (:require [ring.adapter.jetty :as jetty]
            [clojure.core.async :as async]
            [ring.util.response :as response])
  (:use clojure.pprint
        webvim.buffer
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

(defn set-normal-mode[b]
  (-> b 
      (merge {:ex "" :mode normal-mode :keys nil :autocompl {:suggestions nil :suggestions-index 0 :words (-> b :autocompl :words)}})
      history-save))

(defn set-visual-mode[b]
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

(defn save-lastbuf[b]
  (assoc-in b [:context :lastbuf] b))

(defn delete-motion[b]
    (-> b :context :lastbuf 
        buf-save-cursor
        (buf-replace (:cursor b) "")
        history-save))

(defn change-motion[b]
  (-> b :context :lastbuf
      set-insert-mode
      (buf-replace (:cursor b) "")))

(defn visual-mode-select[b]
  (println "visual-mode-select")
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
           :else insert-mode-default })

  (reset! normal-mode-keymap @motion-keymap)
  (swap! normal-mode-keymap 
         merge 
         {"i" (merge 
                {:enter set-insert-mode
                 :leave #(set-normal-mode (cursor-move-char % 0))
                 :continue #(not (= "esc" %2))}
                @insert-mode-keymap)
          "a" (merge
                {:enter set-insert-append
                 :leave set-normal-mode}
                @insert-mode-keymap)
          ":" (merge
                {:enter set-ex-mode
                 :leave set-normal-mode}
                @ex-mode-keymap)
          "u" history-undo
          "c+r" history-redo
          "esc" set-normal-mode
          "v" (merge
                {:enter set-visual-mode
                 :leave set-normal-mode
                 :continue #(not (or (= "esc" %2) (= "v" %2)))
                 :after visual-mode-select}
                @visual-mode-keymap)
          "z" {"z" cursor-center-viewport }
          "d" (merge 
                @motion-keymap
                {:before save-lastbuf 
                 :after delete-motion
                 "d" delete-line})
          "D" delete-line
          "c" (merge
                @motion-keymap
                {:before save-lastbuf 
                 :after change-motion})})
  (reset! root-keymap @normal-mode-keymap))
