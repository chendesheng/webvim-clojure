(ns webvim.core-test
  (:require [clojure.test :refer :all]
            [webvim.keymap :refer :all]
            [webvim.buffer :refer :all])
  (:use clojure.pprint))

(deftest set-visual-mode-test
  (testing "init visual mode"
    (is (let [b (set-visual-mode (buf-insert test-buf "hello"))]
          (and (zero? (-> b :visual :type))
               (check-range b [[0 5] [0 5]]))))))

(deftest visual-mode-move-test
  (testing "move cursor left then check ranges"
    (is (let [_ (init-keymaps)
              h (@visual-mode-keymap "h")
              k (@visual-mode-keymap "k")
              b (k (h (set-visual-mode (buf-insert test-buf "hel\nlo"))))]
          (check-range b [[1 2] [0 1]])))))

(deftest visual-mode-move-cursor-test
  (testing "move cursor left then check cursor"
    (is (let [_ (init-keymaps)
              h (@visual-mode-keymap "h")
              k (@visual-mode-keymap "k")
              b (k (h (set-visual-mode (buf-insert test-buf "hel\nlo"))))]
          (print "cursor::")
          (pprint (-> b :cursor))
          (check-cursor b [0 1 1 0])))))

