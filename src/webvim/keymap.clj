(ns webvim.keymap
  (:require [ring.adapter.jetty :as jetty]
            [clojure.core.async :as async]
            [ring.util.response :as response])
  (:use clojure.pprint
        webvim.buffer))

(def motion-keymap (atom {}))
(def normal-mode-keymap (atom {}))
(def insert-mode-keymap (atom {}))
(def ex-mode-keymap (atom {}))

;enter point of key sequence parser
(defonce active-keymap (atom {}))

;TODO undo/redo history
(defn set-normal-mode[b]
  (reset! active-keymap @normal-mode-keymap)
  (history-save (merge b {:ex "" :mode 0 :keys nil})))

(defn set-insert-mode[b]
  (println "set-insert-mode")
  (reset! active-keymap @insert-mode-keymap)
  (buf-save-cursor (merge b {:ex "" :mode 1 :message nil :keys nil})))

(defn set-insert-append[b]
  (set-insert-mode
    (let [cursor (:cursor b)]
      (if (pos? (:col cursor))
        (let [newcol (-> cursor :col inc)]
          (assoc b :cursor (merge cursor {:col newcol :lastcol newcol})))
        (assoc b :cursor (merge cursor {:col 0 :lastcol 0}))))))

(defn set-ex-mode[b]
  (reset! active-keymap @ex-mode-keymap)
  (merge b {:ex ":" :mode 2 :message nil :keys nil}))

(defn insert-mode-default[b keycode]
  (if (= 1 (count keycode)) 
    (buf-insert b keycode)
    b))

(defn ex-mode-default[b keycode]
  (let [ex (:ex b)]
    (if (= (count keycode) 1)
      (assoc b :ex (str ex keycode))
      b)))

(defn execute [b]
  (let [ex (:ex b)]
    (set-normal-mode (cond
                       (= ex ":w")
                       (write-buffer b)
                       (= ex ":e")
                       (merge (open-file (:name b)) 
                              {:cursor (:cursor b) 
                               :message (str "\"" (:name b) "\" " (count (:lines b)) "L")})
                       :else
                       (assoc b :message "unknown command")))))

;(reset! active-buffer test-buf)

(defn init-keymaps
  "setup keymaps, c+* = ctrl+*; a+* = alt+*. When server recive a keystroke execute function mapped from certain keystroke or :else anything else."
  []
  (reset! motion-keymap
         {"h" #(cursor-move-char % 0)
          "l" #(cursor-move-char % 1)
          "k" #(cursor-move-char % 2)
          "j" #(cursor-move-char % 3)
          "g" {"g" cursor-move-start}
          "G" cursor-move-end
          "c+u" #(cursor-move-viewport %1 -0.5) 
          "c+d" #(cursor-move-viewport %1 0.5)})

  (reset! normal-mode-keymap @motion-keymap)
  (swap! normal-mode-keymap 
         merge {"i" set-insert-mode 
                "a" set-insert-append
                ":" set-ex-mode
                "u" history-undo
                "c+r" history-redo
                "esc" set-normal-mode
                "z" {"z" cursor-center-viewport }})

  (reset! insert-mode-keymap 
          {"esc" #(set-normal-mode (cursor-move-char % 0))
           "backspace" buf-delete
           "c+o" @normal-mode-keymap  ;temp normal mode
           "enter" #(buf-insert % "\n")
           "space" #(buf-insert % " ")
           "tab" #(buf-insert % "\t") 
           :else insert-mode-default })

  (reset! ex-mode-keymap
          {"enter" execute
           "esc" set-normal-mode
           "space" #(assoc % :ex (str (:ex %) " "))
           "backspace" #(let [ex (subs (:ex %) 0 (-> % :ex count dec))]
                         (if (empty? ex)
                           (set-normal-mode %)
                           (assoc % :ex ex)))
           :else ex-mode-default})

  (reset! active-keymap @normal-mode-keymap)) 
