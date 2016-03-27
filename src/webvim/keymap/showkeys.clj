(ns webvim.keymap.showkeys
  (:require [webvim.core.ui :refer [update-ui]])
  (:use clojure.pprint
        webvim.keymap.compile
        webvim.core.rope
        webvim.core.buffer
        webvim.core.event))

(listen
  :before-handle-key
  (fn [buf keycode]
    (update buf :showkeys
            (fn [showkeys]
              (if (and (= (buf :mode) :normal-mode)
                       (or (= (-> buf :visual :type) :no-visual)
                           (= keycode "\"")
                           (and (= (last showkeys) "\"") (not= keycode "<esc>")))
                       (-> buf :line-buffer nil?))
                (conj showkeys keycode))))))

(listen
  :normal-mode-keymap
  (fn [keymap _]
    (-> keymap
        (wrap-key :else
                  (fn [handler]
                    (fn [buf keycode]
                      (if (re-test #"^[0-9]$" keycode)
                        (handler buf keycode)
                        (-> buf
                            (dissoc buf :showkeys)
                            (handler keycode))))))
        (wrap-key :before
                  (fn [handler]
                    (fn [buf keycode]
                      (update (handler buf keycode)
                              :showkeys
                              (fn [showkeys]
                                (if (not= keycode "/")
                                  showkeys))))))
        (wrap-key :after
                  (fn [handler]
                    (fn [buf keycode]
                      (let [buf (handler buf keycode)]
                      ;(println "normal-after:" keycode (-> buf :context :register) (buf :showkeys))
                        (cond
                          (and (= "\"" keycode)
                               (-> buf :context :register nil? not)
                               (-> buf :context :register (not= "\"")))
                          buf
                          (or (re-test #"^[1-9]$" keycode)
                              (and (-> buf :context :repeat-prefix nil? not)
                                   (= keycode "0")))
                          buf
                          (or (= "<esc>" keycode)
                              (-> buf :line-buffer nil? not))
                          (assoc buf :showkeys nil)
                          :else
                          (do
                            (update-ui
                              (fn [ui]
                                (update ui :buf dissoc :showkeys)))
                            (update-buffer (buf :id) 
                                           (fn [buf]
                                             (dissoc buf :showkeys)))
                            (update buf :showkeys conj nil))))))))))

