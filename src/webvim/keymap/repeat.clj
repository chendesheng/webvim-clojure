(ns webvim.keymap.repeat
  (:require [webvim.core.rope :refer [re-test]]
            [webvim.core.event :refer [listen]]
            [webvim.keymap.compile :refer [wrap-key wrap-keycode]]
            [webvim.keymap.operator :refer [if-keycode if-not-keycode]]))

(defn- digit? [keycode]
  (re-test #"^[0-9]$" keycode))

(defn- reset-repeat-prefix [buf keycode]
  (update buf :context dissoc :repeat-prefix))

(defn- append-repeat-prefix [buf keycode]
  (update-in buf [:context :repeat-prefix]
             (fn [count]
               (-> (or count 0)
                   (* 10)
                   (+ (Integer. keycode))))))

(listen :normal-mode-keymap
        (fn [keymap _]
          (-> keymap
              (wrap-key "0" (fn [handler]
                              (fn [buf keycode]
                                (if (-> buf :context :repeat-prefix nil?)
                                  (handler buf keycode)
                                  (append-repeat-prefix buf 0)))))
              (wrap-key :else (fn [handler]
                                (fn [buf keycode]
                                  (-> buf
                                      (handler keycode)
                                      ((if-keycode digit? append-repeat-prefix) keycode)))))
              (wrap-key :after (fn [handler]
                                 (fn [buf keycode]
                                   (-> buf
                                       (handler keycode)
                                       ((if-not-keycode digit? reset-repeat-prefix) keycode))))))))
