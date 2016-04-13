(ns webvim.keymap.repeat
  (:require [webvim.core.rope :refer [re-test]]
            [webvim.core.event :refer [listen]]
            [webvim.keymap.compile :refer [wrap-key wrap-keycode]]))

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

(defn- repeat-prefix? [buf]
  (-> buf :context :repeat-prefix nil? not))

(defn wrap-keymap-repeat-prefix [keymap]
  (-> keymap
      (wrap-key "0" (fn [handler]
                      (fn [buf keycode]
                        (if (repeat-prefix? buf)
                          (append-repeat-prefix buf 0)
                          (handler buf keycode)))))
      (wrap-key :else (fn [handler]
                        (fn [buf keycode]
                          (if (digit? keycode)
                            (append-repeat-prefix buf keycode)
                            (handler buf keycode)))))
      (wrap-key :continue (fn [handler]
                            (fn [buf keycode]
                              (or (and (repeat-prefix? buf)
                                       (digit? keycode))
                                  (handler buf keycode)))))
      (wrap-key :after (fn [handler]
                         (fn [buf keycode]
                           (if (and (repeat-prefix? buf)
                                    (digit? keycode))
                             buf
                             (-> buf
                                 (handler keycode)
                                 (reset-repeat-prefix keycode))))))))

(listen :normal-mode-keymap
        (fn [keymap _]
          (wrap-keymap-repeat-prefix keymap)))
