(ns webvim.motion-test
  (:require [clojure.test :refer :all]
            [webvim.test-util :refer :all]
            [webvim.buffer :refer :all]
            [webvim.global :refer :all])
  (:use clojure.pprint))


(deftest cursor-next-word-test
  (testing ""
    (let [b (-> empty-buf 
                (buf-insert "aaa bbb")
                (assoc :cursor {:row 0 :col 0 :lastcol 0 :vprow 0})
                (cursor-next-word))]
      (is (check-cursor (:cursor b) [0 4 4 0])))))

(deftest cursor-next-word-empty-test
  (testing ""
    (let [b (-> empty-buf 
                (buf-insert "aaa   bbb")
                (assoc :cursor {:row 0 :col 3 :lastcol 3 :vprow 0})
                (cursor-next-word))]
      (is (check-cursor (:cursor b) [0 6 6 0])))))

(deftest cursor-next-word-cross-line-test
  (testing ""
    (let [_ (swap! window assoc :viewport {:w 1 :h 3})
          b (-> empty-buf 
                (buf-insert "aaa   bbb\nccc")
                (assoc :cursor {:row 0 :col 6 :lastcol 6 :vprow 0})
                (cursor-next-word))]
      (is (check-cursor (:cursor b) [1 0 0 1])))))

;(cursor-next-word-cross-line-test)

(deftest cursor-next-word-from-empty-line-test
  (testing ""
    (let [_ (swap! window assoc :viewport {:w 1 :h 3})
          b (-> empty-buf 
                (buf-insert "bbb\n\n@ccc")
                (assoc :cursor {:row 1 :col 0 :lastcol 0 :vprow 0})
                (cursor-next-word))]
      (is (check-cursor (:cursor b) [2 0 0 1])))))

;(cursor-next-word-from-empty-line-test)

(deftest line-back-re-test
  (testing ""
    (let [matched (line-back-re "@hello" 3 re-word-start-back)]
      (is (vector? matched))
      (is (= 1 (matched 0))))))

;(line-back-re-test)

(deftest cursor-back-word-test
  (testing ""
    (let [b (-> empty-buf
                (buf-insert "bbb aaa")
                cursor-back-word)]
      (is (check-cursor (:cursor b) [0 4 4 0])))))

;(cursor-back-word-test)

(deftest cursor-back-word-cross-line-test
  (testing ""
    (let [b (-> empty-buf
                (buf-insert "bbb\n\naaa")
                cursor-back-word
                cursor-back-word)]
      (is (check-cursor (:cursor b) [0 0 0 0])))))


;(cursor-back-re-cross-line-test)

(deftest cursor-next-char-test
  (testing ""
    (let [b (-> empty-buf
                (buf-insert "helloworld")
                (cursor-line-first)
                (cursor-next-char "w"))]
      (is (check-cursor (:cursor b) [0 5 5 0])))))

;(cursor-next-char-test)

(deftest cursor-next-char-not-found-test
  (testing ""
    (let [b (-> empty-buf
                (buf-insert "helloworld")
                (cursor-line-first)
                (cursor-next-char "1"))]
      (is (check-cursor (:cursor b) [0 0 0 0])))))

;(cursor-next-char-not-found-test)

(deftest cursor-back-char-test
  (testing ""
    (let [b (-> empty-buf
                (buf-insert "helloworld")
                (cursor-back-char "w"))]
      (is (check-cursor (:cursor b) [0 5 5 0])))))
;(cursor-back-char-test)

(deftest cursor-back-char-not-found-test
  (testing ""
    (let [b (-> empty-buf
                (buf-insert "helloworld")
                (cursor-back-char "1"))]
      (is (check-cursor (:cursor b) [0 10 10 0])))))

;(cursor-back-char-not-found-test)


(deftest cursor-next-str-test
  (testing ""
    (let [b (-> empty-buf
                (buf-insert "hello\nworld")
                dec-col)
          b1 (cursor-next-str b "\bworld\b")]
      (is (= 1 (-> b1 :cursor :row))))))

;(cursor-next-str-test)


