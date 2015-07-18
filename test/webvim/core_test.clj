(ns webvim.core-test
  (:require [clojure.test :refer :all]
            [webvim.keymap :refer :all]
            [webvim.test-util :refer :all]
            [webvim.buffer :refer :all])
  (:use clojure.pprint))

(deftest open-file-test
  (testing "open a local file return init buffer"
    (is (let [b (open-file "project.clj")]
          (> (-> b :lines count) 0)))))

(deftest replace-range-insert-empty-test
  (testing "insert into empty tests"
   (is (let [lines []]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines [{:row 0 :col 0} {:row 0 :col 0}] "ha\nha")]
           (and (= 2 (count newlines))
                (= "ha" (newlines 0))
                (= 2 (:col newcursor))
                (= 1 (:row newcursor))))))))

(deftest replace-range-insert-line-end-test
  (testing "insert into line end"
   (is (let [lines ["hello"]]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines [{:row 0 :col 5} {:row 0 :col 5}] "haha")]
           (and (= 1 (count newlines))
                (= "hellohaha" (newlines 0))
                (= 9 (:col newcursor))
                (= 0 (:row newcursor))))))))


(deftest replace-range-insert-test
  (testing "insert texts"
   (is (let [lines ["hello" "world"]]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines [{:row 0 :col 0} {:row 0 :col 0}] "haha")]
           (and (= 2 (count newlines))
                (= 4 (:col newcursor))
                (= 0 (:row newcursor))))))))

(deftest replace-range-insert-breakline-test
  (testing "insert test contains \\n"
   (is (let [lines ["hello" "world"]]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines [{:row 0 :col 0} {:row 0 :col 0}] "aa\n\nbb")]
           (and (= 4 (count newlines))
                (= 2 (:col newcursor))
                (= 2 (:row newcursor))))))))

(deftest replace-range-insert-middle-test
  (testing "insert test contains \\n"
   (is (let [lines ["hello" "world" "yes"]]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines [{:row 1 :col 1} {:row 1 :col 1}] "aa\n\nbb")]
           (and (= 5 (count newlines))
                (= 2 (:col newcursor))
                (= 3 (:row newcursor))))))))

(deftest replace-range-insert-last-test
  (testing "insert test contains \\n"
   (is (let [lines ["hello" "world" "yes"]]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines [{:row 1 :col 1} {:row 1 :col 1}] "aa\n\nbb")]
           (and (= 5 (count newlines))
                (= 2 (:col newcursor))
                (= 3 (:row newcursor))))))))


(deftest replace-range-delete-test
  (testing "delete test"
   (is (let [lines ["hello" "world" "yes"]]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines [{:row 0 :col 1} {:row 1 :col 1}] "")]
           (and (= 2 (count newlines))
                (= 1 (:col newcursor))
                (= "horld" (newlines 0))
                (= 0 (:row newcursor))))))))

(deftest replace-range-replace-test
  (testing "delete & insert test"
   (is (let [lines ["hello" "world" "yes"]]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines [{:row 0 :col 1} {:row 1 :col 1}] "yes\n")]
           (and (= 3 (count newlines))
                (= 0 (:col newcursor))
                (= "hyes" (newlines 0))
                (= 1 (:row newcursor))))))))

(deftest buf-insert-test
  (testing "insert to a buffer"
    (let [b (buf-insert empty-buf "hello")]
      (is (check-cursor b [0 5 5 0]))
      (is (= "hello" (-> b :lines (get 0)))))))

(deftest buf-insert-new-line-test
  (testing "check cursor vprow"
    (let [b (buf-insert (assoc empty-buf :viewport {:w 1 :h 2}) "a\n\n\n")]
      (is (check-cursor b [3 0 0 1])))))

(deftest buf-delete-test
  (testing ""
    (let [b (-> empty-buf (buf-insert "hello") (buf-delete))]
      (is (check-cursor b [0 4 4 0])))))

(deftest buf-delete-cross-line-test
  (testing ""
    (let [b (-> empty-buf (assoc :viewport {:w 1 :h 3}) (buf-insert "hello\n\n\n\n") (buf-delete))]
      (is (check-cursor b [3 0 0 1])))))

(deftest buf-delete-up-cross-viewport-test
  (testing ""
    (let [b (-> empty-buf
                (assoc :viewport {:w 1 :h 3})
                (buf-insert "hello\n\n\n\n")
                (buf-replace {:row 1 :col 0 :lastcol 0 :vprow 0} ""))]
      (is (check-cursor b [1 0 0 0])))))

(deftest buf-delete-down-cross-viewport-test
  (testing ""
    (let [b (-> empty-buf
                (assoc :viewport {:w 1 :h 2})
                (buf-insert "hello\n\n\n\n")
                (cursor-move-char 2)
                (cursor-move-char 2)
                (cursor-move-char 2)
                (buf-replace {:row 3 :col 0 :lastcol 0 :vprow 0} ""))]
      (is (check-cursor b [1 0 0 0])))))

;(buf-delete-down-cross-viewport-test)

;(deftest buf-replace-test
;  (testing ""
;    (let [b (-> empty-buf 
;                (assoc :viewport {:w 1 :h 2}) 
;                (buf-insert "hello\n\n\n\n")
;                (buf-replace {:row 0 :col 2 :

(buf-delete-cross-line-test)

(deftest cursor-sort-test
  (testing ""
    (let [[cur1 cur2] (cursor-sort {:row 5 :col 4} {:row 5 :col 5})]
      (is (= 4 (:col cur1))
          (= 5 (:col cur2))))))

(deftest cursor-sort-reverse-test
  (testing ""
    (let [[cur1 cur2] (cursor-sort {:row 5 :col 6} {:row 5 :col 5})]
      (is (= 5 (:col cur1))
          (= 6 (:col cur2))))))

(deftest cursor-sort-different-row-test
  (testing ""
    (let [[cur1 cur2] (cursor-sort {:row 6 :col 6} {:row 5 :col 5})]
      (is (= 5 (:row cur1))
          (= 6 (:col cur1))))))

(deftest history-save-test
  (testing "save history"
    (is (let [b (buf-save-cursor empty-buf)
              b1 (merge b {:lines ["hello" "world"] :cursor {:row 1} :ex "exex"})
              h1 (:history (history-save b1))]
          (and (= 1 (:version h1)) (= 2 (-> h1 :items count)))))))

(deftest history-peek-test
  (testing "peek history"
    (is (map? (history-peek {:history {:items [{:lines {} :cursor {}}] :version 0}})))))

(deftest history-save-unchanged-test
  (testing "save same buffer twice"
    (is (let [b1 (buf-save-cursor (merge empty-buf {:cursor {:row 1} :ex "exex"}))
              h1 (:history (history-save b1))]
          (and (= 0 (:version h1)) 
               (= 1 (-> h1 :items count))
               (= (-> h1 :items first :cursor-end :row) 0))))))

(deftest history-undo-redo-test
  (testing "make changes then undo redo"
    (is (let [b (reduce 
                  #(history-save (buf-insert (buf-save-cursor %1) (str %2)))
                  empty-buf (range 10))
              b1 (history-undo b)
              b2 (history-redo b1)]
          (and (= ((b1 :lines) 0) "012345678")
               (= (b2 :lines) (b :lines))
               (= (b2 :cursor) (b :cursor)))))))

(deftest history-undo-boundary-test
  (testing "undo until init version"
    (is (let [b (buf-save-cursor empty-buf)
              b1 (history-save (buf-insert b "yes"))
              b2 (history-undo (history-undo b1))]
          (and (= (b2 :lines) (b :lines))
               (zero? (-> b2 :cursor :row)))))))

             
(deftest history-undo-cursor-test
  (testing "undo cursor"
    (is (let [b (buf-save-cursor empty-buf)
              b1 (history-save (buf-insert b "yes"))
              b2 (buf-save-cursor b1)
              b3 (history-save (buf-insert b2 "yes"))

              b4 (history-undo (history-undo b3))]
          (and (= (b4 :lines) (b :lines))
               (zero? (-> b4 :cursor :row)))))))

(deftest buffer-word-before-cursor-test
  (testing ""
    (is (let [word (buffer-word-before-cursor (buf-insert empty-buf "yes we can"))]
          (= "can" word)))))

(deftest buffer-word-before-cursor-single-test
  (testing ""
    (is (let [word (buffer-word-before-cursor (buf-insert empty-buf "can"))]
          (= "can" word)))))

(deftest buffer-word-before-cursor-empty-test
  (testing ""
    (is (let [word (buffer-word-before-cursor (buf-insert empty-buf ""))]
          (= "" word)))))

(deftest buffer-word-before-cursor-blank-test
  (testing ""
    (is (let [word (buffer-word-before-cursor (buf-insert empty-buf " "))]
          (= "" word)))))

(deftest buffer-replace-suggestion-test
  (testing ""
    (is (let [b (buffer-replace-suggestion (buf-insert empty-buf "hello") "yayaya")]
          (= "yayaya" (-> b :lines first))))))

