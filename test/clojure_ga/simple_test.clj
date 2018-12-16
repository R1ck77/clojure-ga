(ns clojure-ga.simple-test
  (:require [clojure.test :refer :all]
            [clojure-ga.simple :as simple]
            [clojure-ga.config :as config]))


(defn- throwing-function [& args]
  (is false "unexpected execution"))

(defn- create-throwing-instance []
  (simple/->SimpleGA throwing-function throwing-function throwing-function))


(deftest empty-population
  (testing "returns an empty vector for an empty population"
    (is (= [] (simple/evolve (create-throwing-instance) []))))
  (testing "doesn't invoke any function"
    (simple/evolve (create-throwing-instance) [])
    (is true)))

(deftest single-chromosome
  (testing "a step involves the execution of fitness and mutation operations, in order"
    (let [operations (atom [])]
      (let [fitness-f (fn [chromosome]
                        (swap! operations #(conj % (str chromosome "-fitness")))
                        1)
            mutation-f (fn [chromosome]
                         (swap! operations #(conj % (str chromosome "-mutation"))))
            simpleGa (simple/->SimpleGA fitness-f throwing-function mutation-f)]
       (simple/evolve simpleGa [:a]))
      (is (= [":a-fitness" ":a-mutation"] @operations)))))

(deftest two-chromosomes
  (testing "a step involves the execution of fitness, crossover and mutation, in order"
    (let [operations (atom [])]
      (let [simpleGa (simple/map->SimpleGA {:fitness-f (fn [chromosome]
                                      (swap! operations #(conj % (str chromosome "-fitness")))
                                      1)
                         :cross-f (fn [chromosome-1 chromosome-2]
                                    (swap! operations #(conj % (str chromosome-1 "-crossed-" chromosome-2)))
                                    [chromosome-1 chromosome-2])
                         :mutation-f (fn [chromosome]
                                       (swap! operations #(conj % (str chromosome "-mutation")))
                                       chromosome)})]
        (simple/evolve simpleGa [:a :b]))      
      (is (= [":a-fitness" ":b-fitness" ":a-crossed-:b" ":a-mutation" ":b-mutation"] @operations)))))
