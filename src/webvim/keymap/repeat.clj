(ns webvim.keymap.repeat
  (:require [webvim.core.rope :refer [re-test]]
            [webvim.core.event :refer [listen]]
            [webvim.keymap.compile :refer [wrap-key]]))

(defn- reset-repeat-prefix [buf keycode]
  (if (and
        (-> buf :context :repeat-prefix nil? not)
        (not (re-test #"^[0-9]$" keycode)))
    (update-in buf [:context] dissoc :repeat-prefix)
    buf))

(defn- append-repeat-prefix [buf digit-str]
  (update-in buf [:context :repeat-prefix] #(str % digit-str)))

(listen :normal-mode-keymap
        (fn [keymap _]
          (-> keymap
              (wrap-key "0" (fn [handler]
                              (fn [buf keycode]
                                (if (-> buf :context :repeat-prefix nil? not)
                                  (append-repeat-prefix buf "0")
                                  (handler buf keycode)))))
              (wrap-key :else (fn [handler]
                                (fn [buf keycode]
                                  (if (re-test #"^[0-9]$" keycode)
                                    (-> buf
                                        (handler keycode)
                                        (append-repeat-prefix keycode))
                                    (handler buf keycode)))))
              (wrap-key :after (fn [handler]
                                 (fn [buf keycode]
                                   (-> buf
                                       (handler keycode)
                                       (reset-repeat-prefix keycode))))))))
