(ns webvim.core-test
  (:require [clojure.test :refer :all]
            [webvim.keymap :refer :all]
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
                (= 2 (:lastcol newcursor))
                (= 1 (:row newcursor))))))))

(deftest replace-range-insert-line-end-test
  (testing "insert into line end"
   (is (let [lines ["hello"]]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines [{:row 0 :col 5} {:row 0 :col 5}] "haha")]
           (and (= 1 (count newlines))
                (= "hellohaha" (newlines 0))
                (= 9 (:col newcursor))
                (= 9 (:lastcol newcursor))
                (= 0 (:row newcursor))))))))


(deftest replace-range-insert-test
  (testing "insert texts"
   (is (let [lines ["hello" "world"]]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines [{:row 0 :col 0} {:row 0 :col 0}] "haha")]
           (and (= 2 (count newlines))
                (= 4 (:col newcursor))
                (= 4 (:lastcol newcursor))
                (= 0 (:row newcursor))))))))

(deftest replace-range-insert-breakline-test
  (testing "insert test contains \\n"
   (is (let [lines ["hello" "world"]]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines [{:row 0 :col 0} {:row 0 :col 0}] "aa\n\nbb")]
           (and (= 4 (count newlines))
                (= 2 (:col newcursor))
                (= 2 (:lastcol newcursor))
                (= 2 (:row newcursor))))))))

(deftest replace-range-insert-middle-test
  (testing "insert test contains \\n"
   (is (let [lines ["hello" "world" "yes"]]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines [{:row 1 :col 1} {:row 1 :col 1}] "aa\n\nbb")]
           (and (= 5 (count newlines))
                (= 2 (:col newcursor))
                (= 2 (:lastcol newcursor))
                (= 3 (:row newcursor))))))))

(deftest replace-range-insert-last-test
  (testing "insert test contains \\n"
   (is (let [lines ["hello" "world" "yes"]]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines [{:row 1 :col 1} {:row 1 :col 1}] "aa\n\nbb")]
           (and (= 5 (count newlines))
                (= 2 (:col newcursor))
                (= 2 (:lastcol newcursor))
                (= 3 (:row newcursor))))))))


(deftest replace-range-delete-test
  (testing "delete test"
   (is (let [lines ["hello" "world" "yes"]]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines [{:row 0 :col 1} {:row 1 :col 1}] "")]
           (and (= 2 (count newlines))
                (= 1 (:col newcursor))
                (= 1 (:lastcol newcursor))
                (= "horld" (newlines 0))
                (= 0 (:row newcursor))))))))

(deftest replace-range-replace-test
  (testing "delete & insert test"
   (is (let [lines ["hello" "world" "yes"]]
         (let [{newlines :lines newcursor :cursor} 
               (replace-range lines [{:row 0 :col 1} {:row 1 :col 1}] "yes\n")]
           (and (= 3 (count newlines))
                (= 0 (:col newcursor))
                (= 0 (:lastcol newcursor))
                (= "hyes" (newlines 0))
                (= 1 (:row newcursor))))))))

;(deftest history-save-test
;  (testing "save history"
;    (is (let [b {:cursor {} 
;                 :lines ["hello"]
;                 :history {:items [] :version 0}}
;                b1 (history-save b)
;                h2 (:history (history-save (assoc b1 :lines ["hello" "world"])))]
;          (and (= 2 (:version h2)) (= 2 (-> h2 :items count)))))))

(deftest history-save-unchanged-test
  (testing "save same buffer twice"
    (is (let [b {:cursor {} 
                 :lines "hello"
                 :history {:items [] :version 0}}
              b1 (history-save b)]
          ;h2 (:history (history-save (merge b1 {:cursor {:row 1} :ex "exex"})))]
          (and (= 1 (:version b1)) (= 1 (-> b1 :items count)))))))

(deftest history-save-empty-history-test
  (testing "first time save history"
    (is (let [b {:cursor {} 
                 :lines "hello"
                 :history {:items [] :version 0}}
                h (:history (history-save b))]
          (and (= 1 (:version h)) (= 1 (-> h :items count)))))))
