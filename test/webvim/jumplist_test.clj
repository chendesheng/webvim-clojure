(ns webvim.jumplist-test
  (:require [clojure.test :refer :all]
            [webvim.global :refer :all]
            [webvim.test-util :refer :all]
            [webvim.jumplist :refer :all])
  (:use clojure.pprint))

(def testbuf {:id 1 :lines ["aaa"] :cursor {:row 0 :col 0 :lastcol 0 :vprow 0}})

(deftest jump-push-test
  (testing ""
    (let [b testbuf]
      (reset! jump-list {:positions [] :current 0})
      (jump-push b)
      (jump-push b)
      (is (= 2 (-> @jump-list :positions count)))
      (is (= 2 (-> @jump-list :current))))))

;(jump-push-test)

(deftest jump-prev-test
  (testing ""
    (let [b (assoc-in testbuf [:cursor :row] 1)]
      (reset! jump-list {:positions [] :current 0})
      (jump-push b)
      (let [pos (jump-prev b)]
        (is (check-cursor (pos :cursor) [1 0 0 0]))))))

;(jump-prev-test)

(deftest jump-next-test
  (testing ""
    (let [b (assoc-in testbuf [:cursor :row] 1)]
      (reset! jump-list {:positions [] :current 0})
      (jump-push b)
      (jump-push b)
      (let [pos (jump-prev b)
            _ (jump-prev b)
            pos1 (jump-next b)]
        (is (= pos pos1))))))

;(jump-next-test)

