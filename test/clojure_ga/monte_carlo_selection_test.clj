(ns clojure-ga.monte-carlo-selection-test
  (require [clojure.test :refer :all]
           [clojure-ga.monte-carlo-selection :as selection]))

(deftest test-select
  (testing "select from an empty population returns an empty list"
    (is (= [] (selection/select 10 [] #(identity %) (fn [] 12))))))
