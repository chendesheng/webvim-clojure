(ns webvim.core.editor
  (:require [webvim.core.event :refer [fire-event]]
            [me.raynes.fs :as fs]
            [webvim.core.utils :refer [uuid]]
            [webvim.core.views]))

(defonce editor (atom {:windows {}}))

(defn- create-window []
  (fire-event {:id (uuid)
               :cwd (str fs/*cwd*)
               :active-buffer nil
               :viewport {:w 0 :h 0}
               :registers {}} :create-window))

(defn get-or-create-window [id]
  (or (-> @editor :windows (get id))
      (let [{id :id :as window} (create-window)
            awindow (agent window
                           :error-handler
                           (fn [window err]
                             (println "window agent failed:" err)))]
        (swap! editor assoc-in [:windows id] awindow)
        awindow)))

(defn update-buffer [old-window bufid fn-update]
  (let [old-buf (-> old-window :buffers (get bufid))
        {window :window
         bufid :id
         :as buf} (-> old-buf
                      ;The handler is buffer object centric
                      ;Change :window when need to update the window object;
                      (assoc :window old-window)
                      fn-update)
        ;FIXME: confusing event name, there is a :change-buffer event which fires when :str changes
        new-buf (fire-event (dissoc buf :window) old-buf window :buffer-changed)
        new-window (update-in window
                              [:buffers bufid]
                              #(if (some? %) new-buf))]
    (fire-event new-window old-window nil :window-changed)))

(defn async-update [winid fn-update]
  (send (get-or-create-window winid) fn-update))

(defn async-update-buffer
  ([winid bufid fn-update]
    (send (get-or-create-window winid) update-buffer bufid fn-update))
  ([{{winid :id} :window bufid :id :as buf} fn-update]
    (async-update-buffer winid bufid fn-update)
    buf))

