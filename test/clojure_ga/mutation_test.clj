(ns clojure-ga.mutation-test
  (:require [clojure.test :refer :all]
            [clojure-ga.utils :as utils]
            [clojure-ga.mutation :as mutation]))

(deftest mutation-empty-list
  (testing "returns an empty vector if the population is empty"
    (is (= []
           (mutation/mutation [] (fn [_])))))
  (testing "doesn't invoke the mutation operator if the population is empty"
    (is (= []
           (mutation/mutation [] (fn [_] (is false "mutation function executed unexpectedly")))))))

(deftest mutation-many-elements
  (testing "maps the mutation operator on all elements. Yes. It's a map operation, period :p"
    (is (= [:a :b :c :d]
           (mutation/mutation ["a" "b" "c" "d"] keyword)))))
