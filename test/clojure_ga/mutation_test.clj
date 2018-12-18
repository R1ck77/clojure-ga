(ns clojure-ga.mutation-test
  (:require [clojure.test :refer :all]
            [clojure-ga.utils :as utils]
            [clojure-ga.mutation :as mutation]))

(deftest mutation-empty-list
  (testing "returns an empty vector if the population is empty"
    (is (= []
           (mutation/mutate (mutation/->SimpleMutation (fn [_])) [] ))))
  (testing "doesn't invoke the mutation operator if the population is empty"
    (is (= []
           (mutation/mutate (mutation/->SimpleMutation (fn [_] (is false "mutation function executed unexpectedly")))  [] )))))

(deftest mutation-many-elements
  (testing "maps the mutation operator on all elements. Yes. It's a map operation, period :p"
    (is (= [:a :b :c :d]
           (mutation/mutate (mutation/->SimpleMutation keyword) ["a" "b" "c" "d"])))))

(deftest vector-mutation
  (testing "general case, different probabilities"
    (is (= [":a" :b :c ":d" :e :f]
           (mutation/mutate (mutation/create-vector-mutation (fn [chromosome]
                                                               (str chromosome))
                                                             0.5
                                                             (utils/create-iterator [0.2 0.5 0.8 0.1 0.9 1.0]))
                            [:a :b :c :d :e :f])))))
