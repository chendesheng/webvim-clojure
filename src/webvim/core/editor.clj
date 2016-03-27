(ns webvim.core.editor
  (:require [webvim.core.event :refer [fire-event]]
            [webvim.core.utils :refer [uuid]]))


(defonce *window* {:id (uuid)
                   :buffers (atom {})
                   :jumplist (atom {})})
