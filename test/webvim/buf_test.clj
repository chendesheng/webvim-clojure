(ns webvim.buf-test
  (:require [clojure.test :refer :all]
            [webvim.keymap :refer :all]
            [webvim.history :refer :all]
            [webvim.cursor :refer :all]
            [webvim.global :refer :all]
            [webvim.test-util :refer :all]
            [webvim.core :refer :all]
            [webvim.buffer :refer :all])
  (:use clojure.pprint))

(deftest smart-indent-clojure-parenthesis-test
  (testing ""
    (let [b (-> empty-buf
                (buf-insert "(hello\nhello"))
          indent (smart-indent-clojure (b :lines) 1)]
      (is (= tab-size indent)))))

;(smart-indent-clojure-parenthesis-test)

(deftest smart-indent-clojure-bracket-test
  (testing ""
    (let [b (-> empty-buf
                (buf-insert "[hello\nhello"))
          indent (smart-indent-clojure (b :lines) 1)]
      (is (= 1 indent)))))

;(smart-indent-clojure-bracket-test)

(deftest smart-indent-clojure-equal-test
  (testing ""
    (let [b (-> empty-buf
                (buf-insert "(defn helloworld[]\n   (println \"hello\")\nhello"))
          indent1 (smart-indent-clojure (b :lines) 2)
          indent2 (smart-indent-clojure (b :lines) 1)]
      (is (= 3 indent1))
      (is (= tab-size indent2)))))

;(smart-indent-clojure-equal-test)
(deftest smart-indent-clojure-right-test
  (testing ""
    (let [b (-> empty-buf
                (buf-insert 
;0 (defn helloworld[]
;1 ..(let[x 1
;2 .......y 2]
;3 ....(println x)
;4 ....(println y)))
"(defn helloworld[]\n(let[x 1\ny 2]\n(println x)\n(println y)))"))
          result [0 tab-size 7 (* 2 tab-size) (* 2 tab-size)]]
      (reduce (fn[buf i]
                (println "row:" i)
                (let [indent (smart-indent-clojure (buf :lines) i)]
                  (is (= (result i) indent))
                  (pprint2 (update-in buf [:lines i] #(str (repeat-space indent) %)) "buf:")))
              b (range 1 5)))))

;(smart-indent-clojure-right-test)


