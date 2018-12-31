(ns clojure-ga.statistics-test
  (:require [clojure.test :refer :all]
            [clojure-ga.statistics :as stats]))

(defn- about [x y eps]
  (<= (Math/abs (- x y)) eps))

(deftest about-test
  (testing "couple of basic cases"
    (is (about 1 2 2))
    (is (not (about 1 2 0.5)))
    (is (about 1 1 0))))

(deftest mean-std-dev-test
  (testing "throws if there are less than two values"
    (is (thrown? ArithmeticException (stats/mean-std-dev [])))
    (is (thrown? ArithmeticException (stats/mean-std-dev [12]))))
  (testing "the average for a repeated value is the value itself"
    (is (= 12 (first (stats/mean-std-dev [12 12]))))
    (is (= 12 (first (stats/mean-std-dev [12 12 12 12 12])))))
  (testing "average, general case"
    (is (= 4 (first (stats/mean-std-dev [2 4 6])))))
  (testing "the standard deviation of numbers all the same is 0"
    (is (= 0.0 (second (stats/mean-std-dev [12 12]))))
    (is (= 0.0 (second (stats/mean-std-dev [12 12 12 12 12])))))
  (testing "the standard deviation doesn't depend from order"
    (let [a-result (second (stats/mean-std-dev [1 2 3 4 5]))]
      (is (about a-result (second (stats/mean-std-dev [5 4 3 2 1])) 1e-5))
      (is (about a-result (second (stats/mean-std-dev [4 5 3 1 2])) 1e-5))))
  (testing "general case"
    (is (about 1.5811388300841898 (second (stats/mean-std-dev [1 2 3 4 5])) 1e-6))))

