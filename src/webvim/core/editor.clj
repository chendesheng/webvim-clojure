(ns webvim.core.editor
  (:require [webvim.core.event :refer [fire-event]]
            [webvim.core.utils :refer [uuid]]))

(defonce editor (atom {:windows {}}))

(defonce *window* {:id (uuid)
                   :buffers (atom {})
                   :jumplist (atom {})
                   :ui (agent {:viewport {:w 0 :h 0}
                               :render! (fn [a b] a)} :error-handler (fn [ui err]
                                                                       (println "ui agent fail:")
                                                                       (println ":bufid " (-> ui :buf :id))
                                                                       (println ":filepath " (-> ui :buf :filepath))
                                                                       (println err)))})
