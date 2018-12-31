(ns clojure-ga.statistics-test
  (:require [clojure.test :refer :all]
            [clojure-ga.statistics :as stats]))

(deftest mean-std-dev-test
  (testing "throws if there are less than two values"
    (is (thrown? ArithmeticException (stats/mean-std-dev [])))
    (is (thrown? ArithmeticException (stats/mean-std-dev [12]))))
  (testing "the average for a repeated value is the value itself"
    (is (= 12 (first (stats/mean-std-dev [12 12]))))
    (is (= 12 (first (stats/mean-std-dev [12 12 12 12 12])))))
  (testing "average, general case"
    (is (= 4 (first (stats/mean-std-dev [2 4 6]))))))

