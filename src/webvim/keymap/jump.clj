(ns webvim.keymap.jump
  (:require
    [me.raynes.fs :as fs]
    [webvim.core.buffer :refer [change-active-buffer get-buffer-by-id]]
    [webvim.keymap.compile :refer [wrap-keycode]]
    [webvim.panel :refer [edit-file output-panel goto-buf]]
    [webvim.core.rope :refer [buf-set-pos subr re-test]]
    [webvim.core.pos :refer [pos-re+ pos-re-]]
    [webvim.core.register :refer [registers-get]]
    [webvim.core.utils :refer [parse-int deep-merge expand-path]]
    [webvim.jumplist :refer [jump-prev jump-next]]))

(defn- move-to-jumplist
  [fndir]
  (fn [buf keycode]
    (loop [pos (fndir buf)]  ;TODO: filter lazy seq instead of loop
      (if (nil? pos)
        buf ;newest or oldest
        (let [newbuf (get-buffer-by-id (pos :id))]
          (if (nil? newbuf)
            ;buffer has been deleted, ignore
            (recur (fndir buf))
            ;pos is avaliable
            (if (< (pos :pos) (count (newbuf :str)))
              (let [id (buf :id)
                    newid (pos :id)
                    newpos (pos :pos)]
                (if (= newid id)
                  ;update pos inside current buffer
                  (buf-set-pos buf newpos)
                  (let []
                    (change-active-buffer id newid)
                    (assoc buf :nextid newid))))
              ;buffer has been modifed and cursor is no longer inside, ignore
              (recur (fndir buf)))))))))

(defn- path-under-cursor [buf]
  (let [r (buf :str)
        pos (buf :pos)
        filename-black-list "\\s:?%*|\"'<>"
        re-end (re-pattern (str "(?m)[^" filename-black-list "](:\\d+)?(?=[" filename-black-list "]|$)"))
        re-start (re-pattern (str "(?m)(?<=[" filename-black-list "]|^)[^" filename-black-list "]"))
        [_ end] (pos-re+ r pos re-end)
        [start _] (pos-re- r pos re-start)
        driver (let [driver (subr r (- start 2) start)]
                 (if (re-test #"[a-zA-Z]:" driver) driver ""))
        [[_ uri _ linenum]] (re-seq #"(([a-zA-Z]:)?[^:]+)(:\d+)?" (str driver (subr r start end)))]
    [uri linenum]))

(defn- goto-file [buf]
  (let [[uri linenum] (path-under-cursor buf)]
    (cond
      (-> uri expand-path fs/exists? not)
      (-> buf
          (assoc :message "!!!file or directory not found!!!")
          (assoc :beep true))
      (nil? linenum)
      (edit-file buf uri false)
      :else
      (edit-file buf uri (parse-int linenum) false))))

(defn wrap-keymap-jump [keymap]
  (-> keymap
      (update "g" assoc "f" (wrap-keycode goto-file))
      (assoc
        "<f1>" (wrap-keycode #(goto-buf % (@(output-panel false) :id)))
        "<c-s-6>" (fn [buf keycode]
                    (let [reg (registers-get "#")]
                      (if (nil? reg)
                        (assoc buf :message "No alternative file")
                        (goto-buf buf (:id (get-buffer-by-id (:id reg)))))))
        "<c-o>" (move-to-jumplist jump-prev)
        ;<tab> === <c-i>
        "<tab>" (move-to-jumplist jump-next))))
