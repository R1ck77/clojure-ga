(ns clojure-ga.simple-test
  (:require [clojure.test :refer :all]
            [clojure-ga.simple :as simple]
            [clojure-ga.config :as config]))

(defn- counter-function
  "Returns a function that just increases the counter"
  [counter]
  (fn [& args]
    (swap! counter inc)))

(defn- throwing-function [& args]
  (is false "unexpected execution"))

(defn- create-mock-config [counter]
  (config/create-config :op-generation (counter-function counter)
                        :op-fitness (counter-function counter)
                        :op-cross (counter-function counter)
                        :op-mutation (counter-function counter)))

(deftest empty-population
  (testing "returns an empty vector for an empty population"
    (is (= [] (simple/evolve [] {:mutation-f throwing-function
                                 :cross-f throwing-function
                                 :fitness-f throwing-function
                                 :random-f throwing-function}))))
  (testing "doesn't invoke any function"
    (simple/evolve [] {:mutation-f throwing-function
                       :cross-f throwing-function
                       :fitness-f throwing-function
                       :random-f throwing-function})
    (is true)))

(deftest single-chromosome
  (testing "a step involves the execution of fitness and mutation operations, in order"
    (let [operations (atom [])]
      (simple/evolve [:a] {:fitness-f (fn [chromosome]
                                        (swap! operations #(conj % (str chromosome "-fitness")))
                                        1)
                           :cross-f throwing-function
                           :mutation-f (fn [chromosome]
                                         (swap! operations #(conj % (str chromosome "-mutation"))))
                           :random-f (fn [] 0.5)})
      (is (= [":a-fitness" ":a-mutation"] @operations)))))

(deftest two-chromosomes
  (testing "a step involves the execution of fitness, crossover and mutation, in order"
    (let [operations (atom [])]
      (simple/evolve [:a :b] {:fitness-f (fn [chromosome]
                                        (swap! operations #(conj % (str chromosome "-fitness")))
                                        1)
                              :cross-f (fn [chromosome-1 chromosome-2]
                                         (swap! operations #(conj % (str chromosome-1 "-crossed-" chromosome-2)))
                                         [chromosome-1 chromosome-2])
                           :mutation-f (fn [chromosome]
                                         (swap! operations #(conj % (str chromosome "-mutation")))
                                         chromosome)
                           :random-f (fn [] 0.5)})
      (is (= [":a-fitness" ":b-fitness" ":a-crossed-:b" ":a-mutation" ":b-mutation"] @operations)))))
