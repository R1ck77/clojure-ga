(ns clojure-ga.simple-test
  (:require [clojure.test :refer :all]
            [clojure-ga.simple :as simple]
            [clojure-ga.config :as config]
            [clojure-ga.fitness-proportionate-selection :as selection]
            [clojure-ga.crossover :as crossover]
            [clojure-ga.mutation :as mutation]))

(deftest evolve
  (testing "each evolve filters the population through selection, crossing and mutation, in order"
    (let [simple-ga (simple/->SimpleGA (reify selection/Selector
                                         (select [this population]
                                           (is (= [:initial-population] population))
                                           [:selected-population]))
                                       (reify crossover/Crossover
                                         (combine [this population]
                                           (is (= [:selected-population] population))
                                           [:bred-population]))
                                       (reify mutation/Mutation
                                         (mutate [this population]
                                           (is (= [:bred-population] population))
                                           [:mutated-population])))]
      (is (= [:mutated-population]
             (simple/evolve simple-ga [:initial-population]))))))
