(ns webvim.keymap-test
  (:require [clojure.test :refer :all]
            [webvim.keymap :refer :all]
            [webvim.test-util :refer :all]
            [webvim.buffer :refer :all])
  (:use clojure.pprint))

(deftest set-visual-mode-test
  (testing "init visual mode"
    (let [b (set-visual-mode (buf-insert empty-buf "hello"))]
      (is (zero? (-> b :visual :type)))
      (is (check-range (-> b :visual :ranges) [[0 5] [0 6]])))))

;(set-visual-mode-test)

(deftest visual-mode-move-test
  (testing "move cursor left then check ranges"
    (let [_ (init-keymap-tree)
          h (@visual-mode-keymap "h")
          k (@visual-mode-keymap "k")
          b (visual-mode-select 
              (k (h (set-visual-mode (buf-insert empty-buf "hel\nlo")))) "a")]
      (is (check-range (-> b :visual :ranges) [[1 2] [0 2]])))))

;(visual-mode-move-test)

(deftest visual-mode-move-cursor-test
  (testing "move cursor left then check cursor"
    (let [_ (init-keymap-tree)
          h (@visual-mode-keymap "h")
          k (@visual-mode-keymap "k")
          b (k (h (set-visual-mode (buf-insert empty-buf "hel\nlo"))))]
      (is (check-cursor (:cursor b) [0 1 1 0])))))
;(visual-mode-move-cursor-test)
