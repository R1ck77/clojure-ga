(ns clojure-ga.crossover-test
  (:require [clojure.test :refer :all]
            [clojure-ga.utils :as utils]
            [clojure-ga.crossover :as crossover]))

(deftest not-enough-elements
  (testing "doesn't invoke the cross operator on an empty population"
    (crossover/crossover [] (fn [_ _] (is false "operator invoked but not expected")))
    (is true))
  (testing "returns an empty vector on an empty population"
    (is (= []
           (crossover/crossover [] (fn [_ _] (is false "operator invoked but not expected"))))))  
  (testing "doesn't invoke the cross operator on a population of one element"
    (crossover/crossover [:chromosome] (fn [_ _] (is false "operator invoked but not expected"))))
  (testing "returns an empty vector on a population of one element"
    (is (= []
           (crossover/crossover [:chromosome] (fn [_ _] (is false "operator invoked but not expected")))))))

(defn- crossover-on-two-elements [counter p-cross expected-random-value]
  (let [conversion {:chromosome1 :result1
                    :chromosome2 :result2}]
    (crossover/crossover [:chromosome1 :chromosome2]
                         (fn [a b]
                           (swap! counter inc)
                           (is (and (= :chromosome1 a)
                                    (= :chromosome2 b)))
                           (vector (get conversion a)
                                   (get conversion b))))))
(deftest one-pair
  (testing "crossover applies the operator once on the chromosomes"
    (let [counter (atom 0)]
      (crossover-on-two-elements counter 1 0.5)
      (is (= 1 @counter) "Crossover operator not executed the expected number of times")))
  (testing "for two chromosomes returns the output of the crossover function"
    (let [counter (atom 0)]
      (is (= [:result1 :result2]
             (crossover-on-two-elements counter 1 1))))))

(deftest multiple-values
  (testing "general case: multiple chromosomes"
    (is (= [11 -8 13 -6 15 -4 17 -2]
           (crossover/crossover [1 2 3 4 5 6 7 8]
                                (fn [a b] (vector (+ a 10) (- b 10))))))))

