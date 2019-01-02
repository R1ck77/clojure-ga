(ns clojure-ga.tsp.demo-test
  (:require [clojure.test :refer :all]
            [clojure-ga.utils :as utils]
            [clojure-ga.tsp.demo :as tsp]))

(deftest test-gen-cities
  (testing "throws illegal argument exception for less than 2 cities"
    (is (thrown? IllegalArgumentException (tsp/gen-cities -1)))
    (is (thrown? IllegalArgumentException (tsp/gen-cities 0)))
    (is (thrown? IllegalArgumentException (tsp/gen-cities 1))))
  (testing "generates a number of cities in the 0-1 interval"
    (is (= [[0 0.025]
            [0.05 0.075]
            [0.1 0.125]
            [0.15 0.175]]
           (tsp/gen-cities 4 (utils/create-iterator [0 0.025
                                                     0.05 0.075
                                                     0.1 0.125
                                                     0.15 0.175]))))))
(deftest test-distance
  (testing "distance between two cities"
    (is (= 1.0 (tsp/travel-length [[0 0] [1 0]] [0 1])))
    (is (= 1.0 (tsp/travel-length [[0 0] [1 0]] [1 0]))))
  (testing "distance between three cities, in different order"
    (is (= 20.0 (tsp/travel-length [[0 0] [0 10] [10 10]] [0 1 2])))
    (is (= (+ (* 10 (Math/sqrt 2)) 10) (tsp/travel-length [[0 0] [0 10] [10 10]] [0 2 1])))))
