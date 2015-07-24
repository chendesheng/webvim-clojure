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
   (is (let [lines [""]]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines {:row 0 :col 0} {:row 0 :col 0} "ha\nha")]
           (and (= 2 (count newlines))
                (= "ha\n" (newlines 0))
                (= 2 (:col newcursor))
                (= 1 (:row newcursor))))))))

;(replace-range-insert-empty-test)

(deftest replace-range-insert-line-end-test
  (testing "insert into line end"
   (is (let [lines ["hello\n"]]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines {:row 0 :col 5} {:row 0 :col 5} "haha")]
           (and (= 1 (count newlines))
                (= "hellohaha\n" (newlines 0))
                (= 9 (:col newcursor))
                (= 0 (:row newcursor))))))))

;(replace-range-insert-line-end-test)


(deftest replace-range-insert-test
  (testing "insert texts"
    (let [lines ["hello\n" "world"]
          {newlines :lines newcursor :cursor} (replace-range lines {:row 0 :col 0} {:row 0 :col 0} "haha")]
      (is (= "hahahello\n" (newlines 0)))
      (is (= 2 (count newlines)))
      (is (= 4 (:col newcursor)))
      (is (= 0 (:row newcursor))))))

;(replace-range-insert-test)

(deftest replace-range-insert-breakline-test
  (testing "insert test contains \\n"
   (is (let [lines ["hello\n" "world"]]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines {:row 0 :col 0} {:row 0 :col 0} "aa\n\nbb")]
           (and (= 4 (count newlines))
                (= 2 (:col newcursor))
                (= 2 (:row newcursor))))))))

;(replace-range-insert-breakline-test)

(deftest replace-range-insert-middle-test
  (testing "insert test contains \\n"
   (is (let [lines ["hello" "world" "yes"]]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines {:row 1 :col 1} {:row 1 :col 1} "aa\n\nbb")]
           (and (= 5 (count newlines))
                (= 2 (:col newcursor))
                (= 3 (:row newcursor))))))))
;(replace-range-insert-middle-test)

(deftest replace-range-insert-last-test
  (testing "insert test contains \\n"
   (is (let [lines ["hello\n" "world\n" "yes"]]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines {:row 1 :col 1} {:row 1 :col 1} "aa\n\nbb")]
           (and (= 5 (count newlines))
                (= 2 (:col newcursor))
                (= 3 (:row newcursor))))))))
;(replace-range-insert-last-test)

(deftest replace-range-delete-test
  (testing "delete test"
    (let [lines ["hello\n" "world\n" "yes"]
          {newlines :lines newcursor :cursor} 
          (replace-range lines {:row 0 :col 1} {:row 1 :col 1} "")]
      (is (= 2 (count newlines)))
      (is (= 1 (:col newcursor)))
      (is (= "horld\n" (newlines 0)))
      (is (= 0 (:row newcursor))))))
;(replace-range-delete-test)


(deftest replace-range-delete-cross-line-test
  (testing "delete test"
    (let [lines ["hello\n" "world"]
          {newlines :lines newcursor :cursor} 
          (replace-range lines {:row 0 :col 5} {:row 1 :col 0} "")]
      (is (= 1 (count newlines)))
      (is (= 5 (:col newcursor)))
      (is (= "helloworld" (newlines 0)))
      (is (= 0 (:row newcursor))))))

;(replace-range-delete-cross-line-test)

(deftest replace-range-replace-test
  (testing "delete & insert test"
    (let [lines ["hello\n" "world\n" "yes"]
          {newlines :lines newcursor :cursor} 
          (replace-range lines {:row 0 :col 1} {:row 1 :col 1} "yes\n")]
      (is (= 3 (count newlines)))
      (is (= 0 (:col newcursor)))
      (is (= "hyes\n" (newlines 0)))
      (is (= "orld\n" (newlines 1)))
      (is (= 1 (:row newcursor))))))

;(replace-range-replace-test)

(deftest buf-insert-test
  (testing "insert to a buffer"
    (let [b (buf-insert empty-buf "hello")]
      (is (check-cursor b [0 5 5 0]))
      (is (= "hello" (-> b :lines (get 0)))))))
;(buf-insert-test)

(deftest buf-insert-new-line-test
  (testing "check cursor vprow"
    (let [_ (swap! window assoc :viewport {:w 1 :h 2})
          b (buf-insert empty-buf "a\n\n\n")]
      (is (check-cursor b [3 0 0 1])))))

;(buf-insert-new-line-test)

(deftest buf-delete-test
  (testing ""
    (let [b (-> empty-buf (buf-insert "hello") (buf-delete))]
      (is (check-cursor b [0 4 4 0])))))

;(buf-delete-test)

(deftest buf-delete-empty
  (testing ""
    (let [b (-> empty-buf (buf-delete))]
      (is (check-cursor b [0 0 0 0]))
      (is (= "" (-> b :lines (get 0)))))))

;(buf-delete-empty)

(deftest buf-delete-cross-line-test
  (testing ""
    (let [_ (swap! window assoc :viewport {:w 1 :h 3})
          b (-> empty-buf 
                (buf-insert "hello\n\n\n\n") 
                (testprint)
                (buf-delete))]
      (is (check-cursor b [3 0 0 1])))))

;(buf-delete-cross-line-test)

(deftest buf-delete-up-cross-viewport-test
  (testing ""
    (let [b (-> empty-buf
                (assoc :viewport {:w 1 :h 3})
                (buf-insert "hello\n\n\n\n")
                (buf-replace {:row 1 :col 0 :lastcol 0 :vprow 0} "" false))]
      (is (check-cursor b [1 0 0 0])))))
;(buf-delete-up-cross-viewport-test)

(deftest buf-delete-down-cross-viewport-test
  (testing ""
    (let [b (-> empty-buf
                (assoc :viewport {:w 1 :h 2})
                (buf-insert "hello\n\n\n\n")
                (cursor-move-char 2)
                (cursor-move-char 2)
                (cursor-move-char 2)
                (buf-replace {:row 3 :col 0 :lastcol 0 :vprow 0} "" false))]
      (is (check-cursor b [1 0 0 0])))))
;(buf-delete-down-cross-viewport-test)

(deftest buf-txt-cache-init-test
  (testing ""
    (let [b (-> (create-buf "test-buf" "hello\nhello"))]
      (is (= (:lines b) (-> b :txt-cache :lines)))
      (is (= "hello\nhello" (-> b :txt-cache :txt))))))

(deftest buf-refresh-txt-cache-test
  (testing ""
    (let [b (-> empty-buf
                (buf-insert "hello\nhello"))]
      (is (not (= (:lines b) (-> b :txt-cache :lines))))
      (let [b1 (buf-refresh-txt-cache b)]
        (is (= (:lines b1) (-> b1 :txt-cache :lines)))
        (is (= "hello\nhello" (-> b1 :txt-cache :txt)))))))


(deftest buf-txt-cache-hit-test
  (testing ""
    (let [b (-> empty-buf
                (buf-insert "hello\nhello"))]
      (is (not (= (:lines b) (-> b :txt-cache :lines))))
      (let [b1 (buf-refresh-txt-cache b)
            b2 (buf-refresh-txt-cache b1)]
        (is (= b1 b2))))))
                
;(buf-delete-down-cross-viewport-test)

;(buf-delete-cross-line-test)

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


(deftest char-under-cursor-test
  (testing ""
    (let [b (-> empty-buf 
                (buf-insert "hello")
                (cursor-move-char 0))
          ch (char-under-cursor b)]
      (is (= "o" ch)))))

;(char-under-cursor-test)

(deftest word-under-cursor-test
  (testing ""
    (let [lines ["hello1 world"]]
      (is (= "hello1") ((word-under-cursor lines {:row 0 :col 0}) 0))
      (is (= "world") ((word-under-cursor lines {:row 0 :col 8}) 0))
      (is (= "world") ((word-under-cursor lines {:row 0 :col 6}) 0))
      (is (= "") ((word-under-cursor lines {:row 0 :col 12}) 0)))))

;(word-under-cursor-test)

(deftest buf-insert-line-after-test
  (testing ""
    (let [b (-> empty-buf
                (buf-insert "he\nllo")
                buf-insert-line-after)]
      (is (= 3 (-> b :lines count))))))

;(buf-insert-line-after-test)
