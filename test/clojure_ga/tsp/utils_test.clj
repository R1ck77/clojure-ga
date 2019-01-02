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
