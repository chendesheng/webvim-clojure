(ns webvim.history
  (:require [me.raynes.fs :as fs]
            [clojure.core.async :as async]
            [clojure.java.io :as io])
  (:use clojure.pprint
        (clojure [string :only (join split)])
        webvim.autocompl
        webvim.global))

(defn history-peek[b]
  ((-> b :history :items) (-> b :history :version)))

(defn history-save
  "push state into history, only :lines and :cursor is needed"
  [b]
  (let [item (history-peek b)]
    (if (not (= (:lines b) (:lines item)))
      (let [version (-> b :history :version)
            newversion (inc version)
            items (subvec (-> b :history :items) 0 newversion)]
        (dissoc (assoc b :history {:items (conj items
                                                {:lines (:lines b) 
                                                 :cursor-begin (:last-cursor b)
                                                 :cursor-end (:cursor b)})
                                   :version newversion}) :last-cursor))
      b)))

;(history-save {:history {:items [] :version 0} :lines [] :cursor {}}) 
;(history-save {:history {:items [{:lines [] :cursor {}}] :version 1} :lines ["aaa"] :cursor {}}) 

(defn history-undo[b]
  (if (pos? (-> b :history :version))
    (let [version (-> b :history :version)
          olditem ((-> b :history :items) version)
          newversion (dec version)
          item ((-> b :history :items) newversion)]
      (merge b {:cursor (:cursor-begin olditem)
                :lines (:lines item)
                :history 
                {:items (-> b :history :items) 
                 :version newversion}}))
    b))

(defn history-redo[b]
  (if (< (-> b :history :version) (-> b :history :items count dec))
    (let [newversion (-> b :history :version inc)
          item ((-> b :history :items) newversion)]
      (merge b {:cursor (:cursor-end item)
                :lines (:lines item)
                :history 
                {:items (-> b :history :items) 
                 :version newversion}}))
    b))


