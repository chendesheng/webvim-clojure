(ns webvim.core.editor
  (:require [webvim.core.event :refer [fire-event]]
            [me.raynes.fs :as fs]
            [webvim.core.utils :refer [uuid]]))

(defonce editor (atom {:windows {}}))

(defonce *window* {:id (uuid)
                   :buffers (atom {})
                   :jumplist (atom {})
                   :cwd (atom (str fs/*cwd*))
                   :ui (agent {:viewport {:w 0 :h 0}
                               :render! (fn [a b] a)} :error-handler (fn [ui err]
                                                                       (println "ui agent fail:")
                                                                       (println ":bufid " (-> ui :buf :id))
                                                                       (println ":filepath " (-> ui :buf :filepath))
                                                                       (println err)))})

(defn current-working-directory []
  @(*window* :cwd))

(defn update-cwd [s]
  (alter-var-root (var fs/*cwd*) (constantly (-> s str fs/file)))
  (reset! (*window* :cwd) (str s)))
