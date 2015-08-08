(ns webvim.autocompl-test
  (:require [clojure.test :refer :all]
            [webvim.autocompl :refer :all]
            [webvim.keymap :refer :all]
            [webvim.test-util :refer :all]
            [webvim.buffer :refer :all])
  (:use clojure.pprint))

(def test-text "So how did we make this mistake? What’s the real nature of this mistake? Right? It looked like we could change memory in place. Right? We were doing it. There’s PEEK and POKE and it looked like we could see that. We could read. But there was nothing about what we were putting in memory that had any correlation to time. Right? It was live again. And now we’re finding, “Well, look at these new computer architectures. Where is the variable?” Well, there’s one version over here from one point-in-time. Right? And another one over here. And that’s on its way to a place that this over there might see at some point. It’s live. Now we see the problem. Right? There are no changing values. There’s values at points in time and all you’re ever going to get is the value for a point-in-time. Right? And values don’t change. Right?
               
    So the biggest key insight of Whitehead was there’s no such thing as a mutable object. We’ve invented them. We need to uninvent them. Okay? And Whitehead’s model, which I am grossly oversimplifying. Okay? I don’t even understand it. The book is completely daunting, but it’s full of really cool insights. And what’s he’s built is a model that says there’s this immutable thing. Then there’s a process in the universe that’s going to create the next immutable thing.
               
    And entities that we see as continuous are a superimposition we place on a bunch of values that are causally-related. We see things happen over time and we say, “Oh, that’s Fred!” or “Oh, that’s the river outside the back of my house” or “That’s a cloud.” Right? We know you can look at a cloud for enough time, and all of a sudden it’s like, well, that was three clouds or the cloud disappeared. Right? There is no cloud changing. Right? You superimpose the notion of cloud on a series of related cloud values.")

(def test-words (autocompl-parse test-text))
(reset! autocompl-words test-words)

(def test-buf (create-buf "Are we there yet?" test-text))

(defn same-seq? [a b]
  (if (= (count a) (count b))
    (every? true? (map #(= %1 %2) a b))
  false))

(deftest autocompl-parse-test
  (testing ""
    (is (-> test-words count pos?))))

(deftest fuzzy-match-all-test
  (testing "match all"
    (is (let [indexes (fuzzy-match "rich" "rich")]
          (same-seq? indexes [0 1 2 3])))))

(deftest fuzzy-match-partial-test
  (testing "match partial"
    (is (let [indexes (fuzzy-match "rich" "rih")]
          (same-seq? indexes [0 1 3])))))

(deftest fuzzy-match-reverse-test
  (testing ""
    (is (let [indexes (fuzzy-match "aaabc" "abc")]
          (same-seq? indexes [2 3 4])))))

(deftest fuzzy-match-not-matched-test
  (testing ""
    (is (let [indexes (fuzzy-match "abdef" "abc")]
          (empty? indexes)))))

(deftest autocompl-suggest-test
  (testing ""
    (let [suggestions (autocompl-suggest test-words "co")]
      (is 
        (same-seq? ["co" "completely" "computer" "continuous" "cool" "correlation" "could" "cloud" "clouds"] suggestions)))))

;(autocompl-suggest-test)

(deftest autocompl-start-test
  (testing ""
    (let [b (autocompl-start (assoc test-buf :cursor {:row 0 :col 100 :lastcol 100}))]
      (is 
        (same-seq? ["co" "completely" "computer" "continuous" "cool" "correlation" "could" "cloud" "clouds"] (-> b :autocompl :suggestions))))))

;(autocompl-start-test)
          
(deftest autocompl-move-inc-test
  (testing ""
    (let [b (autocompl-move (assoc test-buf :cursor {:row 0 :col 100 :lastcol 100}) inc)]
      (is (= 1 (-> b :autocompl :suggestions-index)))
      (is (= "completely" (buffer-word-before-cursor b))))))

;(autocompl-move-inc-test)

(deftest autocompl-move-dec-test
  (testing ""
    (is (let [b (autocompl-move (assoc test-buf :cursor {:row 0 :col 100 :lastcol 100}) dec)]
          (and (= 8 (-> b :autocompl :suggestions-index))
               (= "clouds" (buffer-word-before-cursor b)))))))

(deftest autocompl-move-test
  (testing ""
    (let [b (autocompl-move (assoc test-buf :cursor {:row 0 :col 100 :lastcol 100}) dec)
          b1 (-> b (autocompl-move inc) (autocompl-move inc))]
      (is (= 8 (-> b :autocompl :suggestions-index)))
      (is (= "clouds" (buffer-word-before-cursor b)))
      (is (= 1 (-> b1 :autocompl :suggestions-index)))
      (is (= "completely" (buffer-word-before-cursor b1))))))

;(autocompl-move-test)
