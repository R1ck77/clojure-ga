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
           (mutation/mutate (mutation/create-vector-mutation str 0.5 (utils/create-iterator [0.2 0.5 0.8 0.1 0.9 1.0]))
                            [:a :b :c :d :e :f])))))

(deftest tree-mutation
  (testing "simplest meaningful case"
    (is (= [":x"]
           (mutation/mutate (mutation/create-tree-mutation str 1.0 #(identity 0))
                            [:x]))))
  (testing "mutation inside a list's argument"
    (is (= ['(+ "1" 2)]
           (mutation/mutate (mutation/create-tree-mutation str
                                                           0.5 (utils/create-iterator [1 0.2 1]))
                            ['(+ 1 2)])))
    (is (= ['(+ 1 "2")]
           (mutation/mutate (mutation/create-tree-mutation str
                                                           0.5 (utils/create-iterator [1 1 0.2]))
                            ['(+ 1 2)])))
    (is (= ["(+ 1 2)"]
           (mutation/mutate (mutation/create-tree-mutation str
                                                           0.5 (utils/create-iterator [0]))
                            ['(+ 1 2)]))))
  (comment (testing "general case, different probabilities"
     (is (= ['(Math/sqrt "(* :x :x)" (* :y ":y"))]
            (mutation/mutate (mutation/create-tree-mutation str 0.5 (utils/create-iterator [0.6 0.2 0.8 0.7 0.4]))
                             ['(Math/sqrt (* :x :x) (* :y :y))]))))))
