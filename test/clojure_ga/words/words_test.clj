(ns clojure-ga.words.words-test
  (:require [clojure.test :refer :all]
            [clojure-ga.words.words :as words]))

(defn- fake-rand-int [value]
  (fn [ & _] value))

(deftest test-words-start-seq
  (testing "empty string"
    (is (= [] (words/words-start-seq ""))))
  (testing "single word, various scenarios"
    (is (= [0] (words/words-start-seq "foo")))
    (is (= [0] (words/words-start-seq "foo ")))
    (is (= [1] (words/words-start-seq " foo")))    
    (is (= [1] (words/words-start-seq " foo ")))
    (is (= [2] (words/words-start-seq "  foo   ")))))

(deftest test-pick-random-words-sequence
  (comment (testing "a general case"
     (is (= "gamma delta"
            (words/pick-random-words-sequence "alpha beta gamma delta epsilon" 13 (fake-rand-int 8)))))
   (testing "start from 0"
     (is (= "alpha beta"
            (words/pick-random-words-sequence "alpha beta gamma delta epsilon" 13 (fake-rand-int 0)))))
   (testing "not enough letters"
     (is (= "epsilon"
            (words/pick-random-words-sequence "alpha beta gamma delta epsilon" 100 (fake-rand-int 21)))))
   (testing "out or range"
     (is (= "" (words/pick-random-words-sequence "alpha" 3 (fake-rand-int 1000)))))))

