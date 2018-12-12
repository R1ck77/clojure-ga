(ns clojure-ga.monte-carlo-selection-test
  (require [clojure.test :refer :all]
           [clojure-ga.monte-carlo-selection :as selection]))

(deftest test-select
  (testing "select from an empty population returns an empty list"
    (is (= [] (selection/select 10 [] (fn [x] 12) rand))))
  (testing "selecting from a single value population returns N times the same instance"
    (is (= [:a :a :a :a]  (selection/select 4 [:a] (fn [x] 12) rand)))))
