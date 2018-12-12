(ns clojure-ga.classic-test
  (:require [clojure.test :refer :all]
            [clojure-ga.classic :as classic]
            [clojure-ga.config :as config]
            [clojure-ga.algorithm :as algorithm]
            [clojure-ga.simulator :as simulator])
  (:import [clojure_ga.config PopulationProvider]
           [clojure_ga.classic ClassicGeneticAlgorithm]))

(deftest classic-algorithm-selection
  (testing "The algorithm has a selection phase"
    (is (not (nil? (classic/select (classic/->ClassicGeneticAlgorithm nil)))))))

(deftest classic-algorithm-breed
  (testing "The algorithm has a breed phase"
    (is (not (nil? (classic/breed (classic/->ClassicGeneticAlgorithm nil)))))))

(deftest classic-algorithm-terminate?
  (testing "The algorithm decides when to stop"
    (is (not (nil? (classic/terminate? (classic/->ClassicGeneticAlgorithm nil)))))))

(defn- counter-function
  "Returns a function that just increases the counter"
  [counter]
  (fn [& args]
    (swap! counter inc)))

(defn- create-mock-config [counter]
  (config/create-config :op-generation (counter-function counter)
                        :op-fitness (counter-function counter)
                        :op-cross (counter-function counter)
                        :op-mutation (counter-function counter)))

(deftest classic-algorithm-behavior
  (testing "The classic GA does nothing if it's supposed to terminate immediately"
    (let [counter (atom 0)
          simulation-parameters {:population [:a :b :c]
                                 :config  (create-mock-config counter)}
          algorithm (classic/create-genetic-algorithm 0)]
      (algorithm/advance algorithm simulation-parameters)
      (algorithm/advance algorithm simulation-parameters)
      (algorithm/advance algorithm simulation-parameters)
      (is (zero? @counter)))))
