(ns webvim.keymap.jump
  (:require
    [me.raynes.fs :as fs]
    [webvim.core.buffer :refer [change-active-buffer]]
    [webvim.keymap.compile :refer [wrap-keycode]]
    [webvim.panel :refer [edit-file goto-buf]]
    [webvim.core.rope :refer [buf-set-pos subr re-test]]
    [webvim.core.pos :refer [pos-re+ pos-re-]]
    [webvim.core.register :refer [registers-get]]
    [webvim.core.utils :refer [parse-int deep-merge expand-path]]
    [webvim.core.views :refer [next-view]]
    [webvim.jumplist :refer [jump-prev jump-next jump-current-pos jump-push]]
    [clojure.zip :as zip]))

(defn- print-zip [z]
  (println
    (zip/lefts z)
    (zip/node z)
    (zip/rights z)))

(defn- move-to-jumplist
  [fndir]
  (fn [old-buf keycode]
    (loop [buf (fndir old-buf)]  ;TODO: filter lazy seq instead of loop
      (let [old-loc (jump-current-pos old-buf)
            {next-bufid :id
             next-pos :pos :as loc} (jump-current-pos buf)]
        (cond
          (or (nil? loc) (= old-loc loc))
          old-buf ;newest or oldest
          (= (buf :id) next-bufid)
          (buf-set-pos buf next-pos)
          (-> buf :window :buffers (get next-bufid) nil?)
          ;buffer has been deleted, ignore
          (recur (fndir buf))
          :else
          (goto-buf buf next-bufid))))))

(defn- path-under-cursor [buf]
  (let [r (buf :str)
        pos (buf :pos)
        filename-black-list "\\s:?%*|\"'<>"
        re-end (re-pattern (str "(?m)[^" filename-black-list "](:\\d+)?(?=[" filename-black-list "]|$)"))
        re-start (re-pattern (str "(?m)(?<=[" filename-black-list "]|^)[^" filename-black-list "]"))
        [_ end] (pos-re+ r pos re-end)
        [start _] (pos-re- r pos re-start)
        driver (let [driver (if (>= start 2) (subr r (- start 2) start) "")]
                 (if (re-test #"[a-zA-Z]:" driver) driver ""))
        [[_ uri _ linenum]] (re-seq #"(([a-zA-Z]:)?[^:]+)(:\d+)?" (str driver (subr r start end)))]
    [uri linenum]))

(defn- goto-file [buf]
  (let [[uri linenum] (path-under-cursor buf)]
    (cond
      (-> uri expand-path fs/exists? not)
      (-> buf
          (assoc :message "!!!file or directory not found!!!")
          (assoc :beep? true))
      (nil? linenum)
      (edit-file buf uri false)
      :else
      (edit-file buf uri (parse-int linenum) false))))

(defn wrap-keymap-jump [keymap]
  (-> keymap
      (update "g" assoc "f" (wrap-keycode goto-file))
      (assoc
        ;;"<f1>" (wrap-keycode #(goto-buf % (@(output-panel false) :id)))
        "<c-s-6>" (fn [buf keycode]
                    (let [nextid (-> buf :window :registers (registers-get "#") :id)]
                      (if (nil? nextid)
                        (assoc buf :message "No alternative file")
                        (goto-buf buf nextid))))
        "<c-o>" (move-to-jumplist jump-prev)
        ;<tab> === <c-i>
        "<tab>" (move-to-jumplist jump-next)
        "<c-w>" {"w" (fn [buf _]
                       (println "runkeys: <c-w> w")
                       (-> buf
                           jump-push
                           (update :window next-view (buf :view))))})))
