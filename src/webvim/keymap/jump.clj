(ns webvim.keymap.jump
  (:require
    [webvim.core.buffer :refer [buffer-list change-active-buffer]]
    [webvim.keymap.action :refer [wrap-keycode]]
    [webvim.panel :refer [edit-file output-panel goto-buf]]
    [webvim.core.rope :refer [buf-set-pos subr re-test]]
    [webvim.core.pos :refer [pos-re+ pos-re-]]
    [webvim.core.register :refer [registers-get]]
    [webvim.core.utils :refer [parse-int deep-merge]]
    [webvim.jumplist :refer [jump-prev jump-next]]))

(defn- get-buffer-from-reg [reg]
  (if (nil? reg) nil
      (@buffer-list (reg :id))))

(defn- move-to-jumplist
  [fndir]
  (fn [buf keycode]
    (loop [pos (fndir buf)]  ;TODO: filter lazy seq instead of loop
      (if (nil? pos)
        buf ;newest or oldest
        (let [anewbuf (@buffer-list (pos :id))]
          (if (nil? anewbuf)
            ;buffer has been deleted, ignore
            (recur (fndir buf))
            ;pos is avaliable
            (if (< (pos :pos) (count (@anewbuf :str)))
              (let [id (buf :id)
                    newid (pos :id)
                    newpos (pos :pos)]
                (if (= newid id)
                  ;update pos inside current buffer
                  (buf-set-pos buf newpos)
                  (let []
                    (change-active-buffer id newid)
                    ;(swap! buffer-list update-in [newid] #(buf-set-pos % newpos))
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
    (if (nil? linenum)
      (edit-file buf uri false)
      (edit-file buf uri (parse-int linenum) false))))

(defn wrap-keymap-jump [keymap]
  (-> keymap
      (update-in ["g"] assoc "f" (wrap-keycode goto-file))
      (assoc
        "<f1>" (wrap-keycode #(goto-buf % (output-panel false)))
        "<c-s-6>" (fn [buf keycode]
                    (let [reg (registers-get "#")]
                      (if (nil? reg)
                        (assoc buf :message "No alternative file")
                        (goto-buf buf (get-buffer-from-reg reg)))))
        "<c-o>" (move-to-jumplist jump-prev)
        "<c-i>" (move-to-jumplist jump-next))))
