(ns clojure-ga.mutation-test
  (:require [clojure.test :refer :all]
            [clojure-ga.utils :as utils]
            [clojure-ga.mutation :as mutation]))

(deftest mutation-empty-list
  (testing "mutation returns an empty vector if the population is empty"
    (is (= [] (mutation/mutation [] (fn [_])))))
  (testing "mutation doesn't invoke the mutation operator if the population is empty"
    (is (= [] (mutation/mutation [] (fn [_] (is false "mutation function executed unexpectedly")))))))

(deftest mutation-many-elements)
