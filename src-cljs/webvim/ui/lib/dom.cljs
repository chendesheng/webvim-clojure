(ns webvim.ui.lib.dom)

(enable-console-print!)

(defn $new-buffer [bufid]
  (js/document.body.appendChild
    (doto
     (js/document.createElement "DIV")
      (-> .-id (set! (str "buffer-" bufid)))
      (-> .-className (set! "buffer"))
      (-> .-innerHTML (set! (str "<div id=\"gutter-" bufid "\" class=\"gutter\"></div>"
                                 "<div id=\"content-" bufid "\" class=\"content\">"
                                 "<div id=\"cursor-" bufid "\" class=\"cursor\">&nbsp;</div>"
                                 "<div id=\"selections-" bufid "\" class=\"selections\"></div>"
                                 "<div id=\"highlights-" bufid "\" class=\"highlights\"></div>"
                                 "<div id=\"autocompl-" bufid "\" class=\"autocompl\"></div>"
                                 "<div id=\"cursor-bracket-" bufid "\" class=\"cursor-bracket\"></div>"
                                 "<div id=\"lines-" bufid "\" class=\"lines\"></div>"
                                 "</div>"
                                 "<div id=\"status-bar-" bufid "\" class=\"status-bar\">"
                                 "<span id=\"status-bar-buf-" bufid "\" class=\"ex\"></span>"
                                 "<span id=\"status-bar-cursor-" bufid "\" class=\"cursor\"></span>"
                                 "<span id=\"status-bar-cursor-second-" bufid "\" class=\"cursor second-cursor\"></span>"
                                 "<span id=\"status-bar-keys-" bufid "\" class=\"ongoing-keys\"></span>"
                                 "<span id=\"status-bar-name-" bufid "\" class=\"buf-name\"></span>"
                                 "</div>"
                                 "<div id=\"ex-autocompl-" bufid "\" class=\"autocompl ex-autocompl\"></div>"))))))

(defn- $bufid [prefix bufid]
  (js/document.getElementById (str prefix bufid)))

(defn $lines [bufid]
  ($bufid "lines-" bufid))

(defn $content [bufid]
  ($bufid "content-" bufid))

(defn $gutter [bufid]
  ($bufid "gutter-" bufid))

(defn $statusBar [bufid]
  ($bufid "status-bar-" bufid))

(defn $selections [bufid]
  ($bufid "selections-" bufid))

(defn $highlights [bufid]
  ($bufid "highlights-" bufid))

(defn $cursorBracket [bufid]
  ($bufid "cursor-bracket-" bufid))

(defn $cursor [bufid]
  ($bufid "cursor-" bufid))

(defn $hidden-input []
  (js/document.getElementById "hidden-input"))

(defn $statusBuf [bufid]
  ($bufid "status-bar-buf-" bufid))

(defn $statusKeys [bufid]
  ($bufid "status-bar-keys-" bufid))

(defn $statusName [bufid]
  ($bufid "status-bar-name-" bufid))

(defn $statusCursor [bufid]
  ($bufid "status-bar-cursor-" bufid))

(defn $statusCursorSecond [bufid]
  ($bufid "status-bar-cursor-second-" bufid))

(defn $autocompl [bufid]
  ($bufid "autocompl-" bufid))

(defn $autocomplHeight [bufid]
  (js/document.getElementById (str "autocompl-" bufid "-highlight")))

(defn $exAutocompl [bufid]
  ($bufid "ex-autocompl-" bufid))

(defn $exAutocomplHeight [bufid]
  (js/document.getElementById (str "ex-autocompl-" bufid "-highlight")))

(defn $remove [ele]
  (if (-> ele .-remove some?)
    (.remove ele)
    (if (-> ele .-parentNode some?)
      (-> ele .-parentNode (.removeChild ele))))
  ele)

(defn $empty [ele]
  (loop []
    (when-let [child (.-firstChild ele)]
      (.removeChild ele child)
      (recur))))

(defn $linenum [bufid linenum]
  (js/document.getElementById (str "line-num-" bufid "-" linenum)))

(defn $buffer [bufid]
  (or (js/document.getElementById (str "buffer-" bufid))
      (let [$input ($hidden-input)
            $buf ($new-buffer bufid)
            $cur ($cursor bufid)]
        (if (not= (.-parentNode $input) $cur)
          (do ($remove $input)
              (.appendChild $cur $input)))
        $buf)))

(def request-animation-frame
  (if (-> js/window .-requestAnimationFrame nil?)
    (fn [f]
      (js/setTimeout f (/ 1000.0 60.0)))
    (.-requestAnimationFrame js/window)))

(defn- timer-scroll [ele scrollto i]
  (set! (.-scrollTop ele)
        (if (zero? i)
          scrollto
          (-> ele .-scrollTop (+ scrollto) (* 0.5))))
  (if (pos? i)
    (request-animation-frame #(timer-scroll ele scrollto (dec i)))))

(defn $animateScroll [ele scrollto]
  (timer-scroll ele scrollto 5))

(defn $tabsize [sz]
  (if (pos? sz)
    (set! js/document.body.style.tabSize sz))
  js/document.body.style.tabSize)

(defn $hide [ele]
  (if-not (nil? ele)
    (-> ele .-style .-display (set! "none"))))

(defn $show
  ([ele display]
    (if (some? ele)
      (-> ele .-style .-display (set! display))))
  ([ele]
    ($show ele "")))

(defn $toggle [ele display]
  (if (some? ele)
    ($show ele (if display "block" "none"))))

(defn insert-after [p new-ele target-ele]
  (if (nil? target-ele)
    (.insertBefore p new-ele target-ele)
    (if (= (.-lastChild p) target-ele)
      (.appendChild p new-ele)
      (.insertBefore p new-ele (.-nextSibling target-ele)))))

(defn add-class [ele cls]
  (if (some? ele)
    (-> ele .-classList (.add cls))))

(defn remove-class  [ele cls]
  (if (some? ele)
    (-> ele .-classList (.remove cls))))

(defn toggle-class [ele cls b]
  ((if b add-class remove-class)
    ele cls))

(defn removeUnused [ele usedIds]
  (loop [i (.-firstChild ele)]
    (if-not (nil? i)
      (let [prev i
            i (.-nextSibling i)]
        (if (get usedIds (.-id prev))
          ($remove prev))
        (recur i)))))

(defn get-caret [ele]
  (.-selectionStart ele))

(defn create-span []
  (js/document.createElement "SPAN"))

(def beep
  (let [snd (js/Audio. "data:audio/mpeg;base64,SUQzBAAAAAAAI1RTU0UAAAAPAAADTGF2ZjU1LjIyLjEwMgAAAAAAAAAAAAAA//uUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAASW5mbwAAAAcAAAAMAAATgAAnJycnJycnJzs7Ozs7Ozs7Tk5OTk5OTk5iYmJiYmJiYmJ2dnZ2dnZ2domJiYmJiYmJnZ2dnZ2dnZ2dsbGxsbGxsbHExMTExMTExNjY2NjY2NjY2Ozs7Ozs7Ozs//////////9MYXZmNTUuMjIuMTAyAAAAAAAAAAAkAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA//uUZAAO4plkLxHhU3AmC6aCAAJvCHWOyCkA7cD8MdiAECm4FEWF5ZFvSzJSmKXv/9xuQAf/8CAAAFx5//1PPP/////Z//t8/+3zKn/7oY3PPP9T7ZmYZ3QxXVzDB+RiLFsfiLEWIsRYNgXhYQYN4hxcAoFIKhIAANf//////////V//tnfO/kIxGPdGO5zn0IEABDIQDAzqBucoyHhlwtHA0g1L3f1///3009tvdN/rru/dE9asb0pehisxprNz0ZHcxh+Yq2Y4ojErjImkSIoIlXEQiOkQEikfEYHxIDQ8gWmK6c/6//9f9f/7t6/fsfN30Wnr0dqJQw5mMKkBjnCqcxWdQic4xSyEY0GBE7A8NAuHFsYjoCgFAejM8PR1Cqs0qIfeGECGEBJ8a5n//3//////+///T+////9WajrtojyJphNxoaRLnDLnlI0IGikSTDyrge4ORCQHC4sBIUoAxR3GT6/////X//6r527GvvrRTrzTjFnKiUmHmnXQ//uUZDAP4glkMgNAO3BF7IYxBAduCRWQyA2A7cD1MZlIEB2585nTMNHXMKE7jhQYjIsHR0bDYJTgLiQIwORcNB0QsecE4nEAEBYWMGlTErYDF5hCwY8As1pcv//5/////1///+v/Zr99/S7PRHmV1pVZhQk9UKDU0w9kMMKj5wtQdJwjKiwSxwKgJOBYKhwdJCggaNHL35f////79f+vzLtWd06JaZNm3nMrnFt2mPNJs42Hx4xmqcZONSeOzRuSCNmHxsOuXJA8HA2BAbgGlEQAGoBA8esdAY3Ah8YsWgZuNAIDHTkyyDpPBDgKRA0YUGRpAQZIAiQAAFFUzxbFCdbkWxhMg/2ZCF2tqx6zw53kV/JEhzv7RMgQBiMgfCI4clja6SBx9XHOX/7///xyhb+h4qoXAJ4+GQWAAZKElhz//////6bd116ZOzZrWqlHosm/KyO6IUy0uy1GVVyKggVxBBo8VAosKqKMQg5hEBREUMgiLF8YOwaBg7CVmQuW//uUZFeO80oYtxN5ecA6rHZSBAVuE9x04lXtAAhQAFtCgCAA2Yn5whpxEmGNORWY+4cJi0CjGCSCmYGAKAQAkLBaOsYKYGJgKgsmByBeYKoE5gUgamDKgWWc1acMmbSMEUE7mRr1VtdRdz4uq4UXcmOQ7EqOM1JdMzsqwprtmtfpcLVbtXAqDJosooq2ixlua2x+v4VQoppTSTfNSDSTaKWY41pLubzmP///jerf/+t/6P2bEMqI7//////////////7EvtEJQTiziBCMPaEXDKiBMMzWknGMy6QrTN/JKk+89zxMKsADTAYgT8xWsClMAKA/jAniA4xfsDTMDEAbzAuwA0wN4ASKAJYweIEyMB0BLQMgBwQMow0wM1gxwMQYxgNKo1wMpA1wbwYXU4WMBv4sz8kBShT/83IOkgaf/mZum8w//zSmnTL///uoihUVo///5Nm6i42ym9PSS/XqXebLNrU1vUTBUWbm5odV////+7N////+imYIAqAygOF//uUZFoABXGBNoZ+wABPMCZywJwADixFDrn9AAB1AF8PACACJ/6////76f///rZ/1XuqL////6c5D2R0tTmGo7bLrNPdDLTEHBWZGk6lDDCaBEUDXMSVViomYIwwA4AP////8WhKS/////iMBEDQAQAkxbofwMeaEkjJszWY2DRlxMTFcxz480WowFcGhMApANjHzwDIwEoAZMcXBeDAdQAIHAGqa5gNYCeVQCgwHcAhEQAuhPFgRzxIYkAwEFGZZWvcwqUUQvWwXSBA6GTjsaKmU////sVkU/d7v///SAAKJALLGh/////6CaP1Xo/0B59/igjUWc81/0/VMdsOMzMpFns+BGxoMeO/8TfewmIyKskAMPvB2TB1AKgwacCOMFtCPDBxQEgwOICXMBLAngUBJGALgFhgGIAOBgCEu4HEXOckp6E6IMSYlKjRqma1a9XSFKVmjQYuN7riu/C2XAQCq//v/16TNRL///7/XrAOKP/////9qv//9H/avv16//uUZEIP45wZQQd94AAcYAbS4IgACXAxBg/15MCMMNpEEAm5dPNwE0egKhEEBhEkYZWEbGRTG+J8uhwkaBIygHHriIBiZwNacFlYaaBKZShOYOiyYDiMAAVR2RfakEMPs3wW6bH2q1a9EQ4OiUYeSv///69X+jWS///9TJv///////6p6ft/1//t3+n+u/r6s6TmsidYRZxRDDChTWGKMFSDezC5zXYy49zbMlD+qDEAirAwIMIdMymAoXiw8DAROsMHCgbMMBwaLAVosA2iUCFjsce6xUpMCgIOB8/W/xT////5fkv////VqlAAAAACthSf///////jC8balTLPbq+LataHp9fJCjxa0XEAwwjYGeMSRRAT7Ly/Yzv2t+OYgB2DEqQdswdwC0MEtAozhjcMcHgwOGzFAOCoRdtkbNmAGAgMo+PA8AgIMCzKKkjLLU06KAFQ7Ytujfv/1f/VrJf/3u/3euBP//////ttRt/////pPf/6ajCWQ5YxS5LT//uUZHCP4l4NQYP7wTAjwBbqBAIACvhBBg/xBwBEAB5IAAgGP0eawDGANd05e4VjMRjBWD2CfNJhUzgIDEQUMFCUAhqzAqfDQXSHmEc2Fl43N7+tazv5hfv+N//+v/6GShL/+v/X8qBmCX//////6f9F/dvprW/pvp//pb7e9KWS1FUYRpmGnsVbggsLCjApAyswohG2NsKcnzMbuE8xbUg9MGIChDUtpNGNIzmkzLY5MkBYSKaIkHvS3EuLQdxuCiwDQNHv//////p6P////VKGiX////9r/VP/Stv1//66////6f2bvSyFOjEBqUSKQYlHakxBTUUzLjk5LjWqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqjB6gZkxdMdoP6jI0DKNFTs42MMhMOCAujuZ3NbFAzOSzE45MHBowGB30Q5oxo8gwvjt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VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV//uUZOQA9MlVuotsG3IjjAezAAJvjNSSfySwyUgGAEAA8AAEVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV")]
    #(.play snd)))

(defn $id [id]
  (js/document.getElementById id))

(defn $create [tag]
  (js/document.createElement tag))

(defn $text-content [ele text]
  (set! (.-textContent ele) (str text)))

(defn $hiccup
  ([[tag & children] fn-create fn-content]
    (let [[tag & parts] (re-seq #"(?:#|.)?[^#.]+" (name tag))
          ele (fn-create tag)]
      ;(js/console.log ele)
      (doseq [part parts]
        (case (first part)
          \# (set! (.-id ele) (.substr part 1))
          \. (add-class ele (.substr part 1))))
      (doseq [child children]
        (cond
          (map? child)
          (doseq [[k v] child]
            (if (= k :class)
              (add-class ele v)
              (.setAttribute ele (name k) v)))
          (vector? child)
          (.appendChild ele ($hiccup child fn-create fn-content))
          (string? child)
          ($text-content ele child)
          (some? child)
          (if fn-content
            (fn-content ele child))))
      ele))
  ([node]
    ($hiccup node $create $text-content)))

;(js/console.log ($hiccup [:div.test.second-test
;                          {:id (str "buffer-" 100)}
;                          [:span]
;                          [:span
;                           [:span "hello"]
;                           [:span]]
;                          [:span]
;                          [:span.code {:style "display:none"}]]))

(defn $exist? [id]
  (boolean ($id id)))

(js/console.log
  ($hiccup [:| 100 200 [:- 10 20]]
           (fn [k]
             (cond
               (= k "-") ($hiccup [:div.ver])
               (= k "|") ($hiccup [:div.hor])))
           (fn [ele id]
             (.appendChild ele ($hiccup [:div.buffer {:id (str "buffer-" id)}])))))

(defn client-size []
  [js/window.innerWidth js/window.innerHeight])

(def measure-text-size
  (memoize
    (fn [text]
      (let [sp ($hiccup [:span.code-block text])
            _ (-> sp .-style .-cssText (set! "opacity:0;position:absolute;right:0;bottom:0;"))
            _ (.appendChild js/document.body sp)
            w (.-offsetWidth sp)
            h (.-offsetHeight sp)
            _ (.removeChild js/document.body sp)]
        [w h]))))

(defn line-height []
  (last (measure-text-size "M")))

(defn bounding-rect
  ([ele a b]
    (.getBoundingClientRect (doto (js/document.createRange)
                              (.setStart ele a)
                              (.setEnd ele b))))
  ([ele pos]
    (let [rects (.getClientRects (doto (js/document.createRange)
                                   (.setStart ele pos)
                                   (.setEnd ele pos)))]
      (aget rects (-> rects .-length dec))))
  ([ele pos ele2 pos2]
    (.getBoundingClientRect (doto (js/document.createRange)
                              (.setStart ele pos)
                              (.setEnd ele2 pos2))))
  ([ele]
    (.getBoundingClientRect ele)))

(defn rect-pos [rect]
  [(.-left rect) (.-top rect)])

(defn rect-size [rect]
  [(.-width rect) (.-height rect)])

(defn- text-node? [ele]
  (-> ele .-nodeType (= 3)))

(defn- not-text-node? [ele]
  (-> ele .-nodeType (not= 3)))

(extend-type js/NodeList
  ISeqable
  (-seq [array] (array-seq array 0)))

(defn seq-text-nodes [ele]
  (filter text-node?
          (tree-seq not-text-node? #(-> % .-childNodes) ele)))

(defn get-element-and-offset [$line offset]
  (loop [texts (seq-text-nodes $line)
         i offset]
    (let [ele (first texts)
          seglen (-> ele .-textContent count)]
      (if (< i seglen)
        [ele i]
        (recur (rest texts) (- i seglen))))))

