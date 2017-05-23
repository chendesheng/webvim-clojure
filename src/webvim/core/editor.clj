(ns webvim.core.editor
  (:require [webvim.core.event :refer [fire-event]]
            [me.raynes.fs :as fs]
            [webvim.core.utils :refer [uuid shorten-path]]
            [webvim.core.views]))

(defonce editor (atom {:windows {}}))

(defn- create-window []
  (fire-event {:id (uuid)
               :cwd (-> fs/*cwd* str shorten-path)
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
        ;The handler is buffer object centric
        ;Change :window when need to update the window object;
        input-buf (assoc old-buf :window old-window)
        {window :window bufid :id :as buf} (fn-update input-buf)]
    (if (= input-buf buf)
      old-window ;return old-window if nothing changed
      (let [;FIXME: confusing event name, there is a :change-buffer event which fires when :str changes
            new-buf (fire-event (dissoc buf :window) old-buf window :buffer-changed)
            new-window (update-in window
                                  [:buffers bufid]
                                  #(if (some? %) new-buf))]
        (fire-event new-window old-window nil :window-changed)))))

(defn async-update [winid fn-update]
  (send (get-or-create-window winid) fn-update))

(defn async-update-buffer
  ([winid bufid fn-update]
    (send (get-or-create-window winid) update-buffer bufid fn-update))
  ([{{winid :id} :window bufid :id :as buf} fn-update]
    (async-update-buffer winid bufid fn-update)
    buf))

