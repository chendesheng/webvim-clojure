(ns webvim.ui.lib.xhr)

(defn xhr-get
  ([url success]
    (let [xhr (js/XMLHttpRequest.)]
      (set! (.-onreadystatechange xhr)
            (fn []
              (if (and (= (.-readyState xhr) 4)
                       (= (.-status xhr) 200)
                       success)
                (success))))
      (.open xhr "GET" url true)
      (.send xhr)))
  ([url]
    (xhr-get url nil)))
