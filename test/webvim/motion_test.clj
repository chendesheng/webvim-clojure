(ns webvim.motion-test
  (:require [clojure.test :refer :all]
            [webvim.test-util :refer :all]
            [webvim.buffer :refer :all])
  (:use clojure.pprint))


(deftest cursor-next-word-test
  (testing ""
    (let [b (-> empty-buf 
                (buf-insert "aaa bbb")
                (assoc :cursor {:row 0 :col 0 :lastcol 0 :vprow 0})
                (cursor-next-word))]
      (is (check-cursor b [0 4 4 0])))))

(deftest cursor-next-word-empty-test
  (testing ""
    (let [b (-> empty-buf 
                (buf-insert "aaa   bbb")
                (assoc :cursor {:row 0 :col 3 :lastcol 3 :vprow 0})
                (cursor-next-word))]
      (is (check-cursor b [0 6 6 0])))))

(deftest cursor-next-word-cross-line-test
  (testing ""
    (let [b (-> empty-buf 
                (buf-insert "aaa   bbb\nccc")
                (assoc :cursor {:row 0 :col 6 :lastcol 6 :vprow 0})
                (cursor-next-word))]
      (is (check-cursor b [1 0 0 -1])))))

(deftest cursor-next-word-from-empty-line-test
  (testing ""
    (let [b (-> empty-buf 
                (buf-insert "bbb\n\n@ccc")
                (assoc :cursor {:row 1 :col 0 :lastcol 0 :vprow 0})
                (cursor-next-word))]
      (is (check-cursor b [2 0 0 -1])))))

;(cursor-next-word-from-empty-line-test)

(deftest line-back-re-test
  (testing ""
    (let [[matched col] (line-back-re "@hello" 3 re-word-start-back)]
      (is matched)
      (is (= 1 col)))))

(deftest cursor-back-word-test
  (testing ""
    (let [b (-> empty-buf
                (buf-insert "bbb aaa")
                cursor-back-word)]
      (is (check-cursor b [0 4 4 0])))))

;(cursor-back-word-test)

(deftest cursor-back-word-cross-line-test
  (testing ""
    (let [b (-> empty-buf
                (buf-insert "bbb\n\naaa")
                cursor-back-word
                cursor-back-word)]
      (is (check-cursor b [0 0 0 0])))))


;(cursor-back-re-cross-line-test)

