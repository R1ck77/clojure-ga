(ns clojure-ga.words.words-test
  (:require [clojure.test :refer :all]
            [clojure-ga.words.words :as words]))

(defn- fake-rand-int [value]
  (fn [ & _] value))

(deftest test-words-seq
  (testing "empty string"
    (is (= '() (words/words-seq ""))))
  (testing "single word"
    (is (= '([0 3]) (words/words-seq "foo")))
    (is (= '([0 3]) (words/words-seq "foo ")))
    (is (= '([1 4]) (words/words-seq " foo")))    
    (is (= '([1 4]) (words/words-seq " foo ")))
    (is (= '([2 5]) (words/words-seq "  foo   "))))
  (testing "multiple word"
    (is (= '([0 3] [4 7]) (words/words-seq "foo bar")))
    (is (= '([0 3] [4 7]) (words/words-seq "foo bar ")))
    (is (= '([1 4] [6 9]) (words/words-seq " foo  bar")))    
    (is (= '([1 4] [5 8]) (words/words-seq " foo bar ")))
    (is (= '([2 5] [8 11]) (words/words-seq "  foo   bar  ")))))

(deftest test-pick-random-words-sequence
  (testing "a general case"
    (is (= "gamma delta"
           (words/pick-random-words-sequence "alpha beta gamma delta epsilon" 13 (fake-rand-int 8)))))
  (testing "start from 0"
    (is (= "alpha beta"
           (words/pick-random-words-sequence "alpha beta gamma delta epsilon" 13 (fake-rand-int 0)))))
  (testing "not enough letters"
    (is (= "epsilon"
           (words/pick-random-words-sequence "alpha beta gamma delta epsilon" 100 (fake-rand-int 21)))))
  (testing "out or range"
    (is (= ""
           (words/pick-random-words-sequence "alpha" 3 (fake-rand-int 1000)))))
  (testing "general case"
    (is (= "f g"
           (words/pick-random-words-sequence "a b c d e f g h i j k l" 3 (fake-rand-int 10))))))

(deftest test-clean-text
  (testing "no text"
    (is (= "" (words/clean-text ""))))
  (testing "nothing to clean"
    (is (= "text" (words/clean-text "text"))))
  (testing "nothing remaining"
    (is (= " " (words/clean-text ",^$%*^"))))
  (testing "nothing but spaces remaining"
    (is (= "        "
           (words/clean-text " , ^$% *^  "))))
  (testing "general case"
    (is (= "foo  bar baz  "
           (words/clean-text "foo, bar><baz%^&* ")))))

(deftest test-best-distance
  (testing "both empty"
    (is (= 0 (words/best-distance "" ""))))
  (testing "one is empty"
    (is (= 3 (words/best-distance "" "foo")))
    (is (= 3 (words/best-distance "foo" ""))))
  (testing "two general strings"
    (is (= 1 (words/best-distance "bar" "baz")))
    (is (= 1 (words/best-distance "baz" "bar")))
    (is (= 8 (words/best-distance "is general" "is specific"))))
  (testing "works with any sequence, actually"
    (is (= 8 (words/best-distance [\i \s \space \g \e \n \e \r \a \l]
                                    [\i \s \space \s \p \e \c \i \f \i \c]))))
  (testing "returns the best possible match of strings"
    (is (= 3 (words/best-distance "   foo" "foo")))
    (is (= 4 (words/best-distance "a   foo" "foo")))
    (is (= 4 (words/best-distance "a   foo" "foo")))
    (is (= 4 (words/best-distance "a foobar" "foobar a")))))
