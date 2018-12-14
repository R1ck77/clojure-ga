(ns clojure-ga.crossover-test
  (:require [clojure.test :refer :all]
            [clojure-ga.crossover :as crossover]))

(deftest crossover-behavior
  (testing "crossover doesn't invoke the cross operator on an empty population"
    (crossover/apply []
                     {:random-generator #(throw (IllegalStateException. "Unexpected execution of the random generator"))
                      :cross-probability 1
                      :cross-operator (fn [_ _] (is false "operator invoked but not expected"))})
    (is true))
  (testing "crossover doesn't invoke the cross operator on a population of one element"
    (crossover/apply [:chromosome]
                     {:random-generator #(throw (IllegalStateException. "Unexpected execution of the random generator"))
                      :cross-probability 1
                      :cross-operator (fn [_ _] (is false "operator invoked but not expected"))})))
