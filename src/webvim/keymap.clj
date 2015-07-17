(ns webvim.keymap
  (:require [ring.adapter.jetty :as jetty]
            [clojure.core.async :as async]
            [ring.util.response :as response])
  (:use clojure.pprint
        webvim.buffer
        webvim.autocompl))

(defonce motion-keymap (atom {}))
(defonce normal-mode-keymap (atom {}))
(defonce visual-mode-keymap (atom {}))
(defonce insert-mode-keymap (atom {}))
(defonce ex-mode-keymap (atom {}))

;enter point of key sequence parser
(defonce active-keymap (atom {}))

(defn set-normal-mode[b]
  (reset! active-keymap @normal-mode-keymap)
  (history-save (merge b {:ex "" :mode 0 :keys nil :autocompl {:suggestions nil :suggestions-index 0 :words (-> b :autocompl :words)}})))

(defn set-visual-mode[b]
  (reset! active-keymap @visual-mode-keymap)
  (let [cur (-> b :cursor cursor-to-point)]
    (merge b {:ex "" :mode 3 :keys nil 
              :visual {:type 0 :ranges [cur cur]}})))

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
    (if (empty? (-> b :autocompl :suggestions))
      (assoc-in b1 [:autocompl :suggestions] nil)
      (let [word (buffer-word-before-cursor b1)
            suggestions  (-> b1 :autocompl :words (autocompl-suggest word))]
        (assoc b1 :autocompl 
               (merge (:autocompl b1) 
                      {:suggestions (if (empty? suggestions)
                                      nil
                                      suggestions)
                       :suggestions-index 0}))))))

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
(defn visual-mode-wrap-motions-keymap[keymap]
  (reduce-kv (fn[col k v]
               (assoc col k 
                      (fn[b]
                        (let [newb (v b)]
                          (assoc-in newb [:visual :ranges 1]
                                    (-> newb :cursor cursor-to-point))))))
               {} keymap))

(defn autocompl-start[b]
  (let [word (buffer-word-before-cursor b)
        suggestions (autocompl-suggest (-> b :autocompl :words) word)]
    (assoc b :autocompl 
           (merge (:autocompl b) 
                  {:suggestions suggestions 
                   :suggestions-index 0}))))

(defn autocompl-move[b f]
  (let [b1 (if (nil? (-> b :autocompl :suggestions))
             (autocompl-start b)
             b)
        i (f (-> b1 :autocompl :suggestions-index))
        cnt (-> b1 :autocompl :suggestions count)
        n (mod (+ i cnt) cnt)]
    (-> b1 
        (assoc-in [:autocompl :suggestions-index] n) 
        (buffer-replace-suggestion ((-> b1 :autocompl :suggestions) n)))))

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
         merge 
         {"i" set-insert-mode 
                "a" set-insert-append
                ":" set-ex-mode
                "u" history-undo
                "c+r" history-redo
                "esc" set-normal-mode
                "v" set-visual-mode
                "z" {"z" cursor-center-viewport }})

  (reset! visual-mode-keymap (visual-mode-wrap-motions-keymap @motion-keymap))
  (swap! visual-mode-keymap merge
          {"v" set-normal-mode})

                                    
  (reset! insert-mode-keymap 
          {"esc" #(set-normal-mode (cursor-move-char % 0))
           "c+o" @normal-mode-keymap  ;temp normal mode
           "c+n" #(autocompl-move % inc)
           "c+p" #(autocompl-move % dec)
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
