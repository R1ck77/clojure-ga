(ns clojure-ga.tsp.utils-test
  (:require [clojure.test :refer :all]
            [clojure-ga.tsp.utils :as utils]))

(deftest common-subsets-test
  (testing "general case 1"
    (is (= #{#{:c :b :d} #{:c :b :a}
             #{:c :d} #{:c :b} #{:b :a}}
           (apply hash-set
                  (utils/common-subsets [:a :b :c :d]
                                        [:d :c :b :a])))))
  (testing "general case 2"
    (is (= #{#{:c :b :d}
             #{:c :b} #{:c :d}}
           (apply hash-set
                  (utils/common-subsets [:a :d :c :b]
                                        [:a :b :c :d])))))
  (testing "no common subsets"
    (is (= []
           (utils/common-subsets [:b :d :a :c] [:a :b :c :d])))))

(deftest test-replace-subset
   (testing "2 elements subset, start and end"
    (is (= [[:c :b :a :d] [:d :a :b :c]]
           (utils/replace-subset [:b :c :a :d] [:d :a :c :b] #{:c :b}))))
  (testing "2 elements subset, middle"
    (is (= [[:a :b :c :d] [:d :c :b :a]]
           (utils/replace-subset [:a :c :b :d] [:d :b :c :a] #{:c :b}))))
  (testing "general case"
    (is (= [[:a :b :c :d :e :f]
            [:f :e :d :c :b :a]]
           (utils/replace-subset [:a :e :d :c :b :f]
                                 [:f :b :c :d :e :a]
                                 #{:b :c :d :e})))))
