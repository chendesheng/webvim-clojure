(ns webvim.keymap.repeat
  (:require [webvim.core.rope :refer [re-test]]
            [webvim.core.event :refer [listen log]]
            [webvim.keymap.compile :refer [wrap-key wrap-keycode wrap-continue]]))

(defn- digit? [keycode]
  (re-test #"^[0-9]$" keycode))

(defn- reset-repeat-prefix [buf]
  (update buf :context dissoc :repeat-prefix))

(defn reset-repeat-count [buf]
  (-> buf
      reset-repeat-prefix
      (update :context dissoc :repeat-count)))

(defn- repeat-prefix-value [buf]
  (get-in buf [:context :repeat-prefix] 1))

(defn- append-repeat-prefix [buf keycode]
  (update-in buf [:context :repeat-prefix]
             (fn [count]
               (-> (or count 0)
                   (* 10)
                   (+ (Integer. keycode))))))

(defn repeat-count? [buf]
  (-> buf :context :repeat-prefix nil? not))

(defn repeat-count [buf]
  (* (repeat-prefix-value buf)
     (get-in buf [:context :repeat-count] 1)))

(defn repeat-pos-range [{pos :pos :as buf}]
  [pos (+ pos (repeat-prefix-value buf))])

(defn wrap-keymap-repeat-prefix [keymap]
  (-> keymap
      (wrap-key :enter (fn [handler]
                         (fn [buf keycode]
                           (-> buf
                               (update-in [:context :repeat-count]
                                          #(* (or % 1)
                                              (repeat-prefix-value buf)))
                               reset-repeat-prefix
                               (handler keycode)))))
      (wrap-key "0" (fn [handler]
                      (fn [buf keycode]
                        (if (repeat-count? buf)
                          (append-repeat-prefix buf keycode)
                          (handler buf keycode)))))
      (wrap-key :else (fn [handler]
                        (fn [buf keycode]
                          (if (digit? keycode)
                            (append-repeat-prefix buf keycode)
                            (handler buf keycode)))))
      (wrap-continue (fn [handler]
                       (fn [buf keycode]
                         (or (and (repeat-count? buf)
                                  (digit? keycode))
                             (handler buf keycode)))))
      (wrap-key :after (fn [handler]
                         (fn [buf keycode]
                           (if (and (repeat-count? buf)
                                    (digit? keycode))
                             buf
                             (if (not= (buf :mode) :normal-mode)
                               (handler buf keycode)
                               (-> buf
                                   (handler keycode)
                                   reset-repeat-count))))))))

(listen :normal-mode-keymap
        (fn [keymap _]
          (wrap-keymap-repeat-prefix keymap)))
