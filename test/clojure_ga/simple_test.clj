(ns clojure-ga.simple-test
  (:require [clojure.test :refer :all]
            [clojure-ga.simple :as simple]
            [clojure-ga.config :as config]
            [clojure-ga.algorithm :as algorithm]
            [clojure-ga.simulator :as simulator])
  (:import [clojure_ga.config PopulationProvider]
           [clojure_ga.simple SimpleGeneticAlgorithm]))

(deftest simple-algorithm-breed
  (testing "The algorithm has a breed phase"
    (is (not (nil? (simple/breed (simple/->SimpleGeneticAlgorithm nil) nil))))))

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

(deftest simple-algorithm-behavior
  (testing "The simple GA does nothing if it's supposed to terminate immediately"
    (let [counter (atom 0)
          simulation-parameters {:population [:a :b :c]
                                 :config  (create-mock-config counter)}
          algorithm (simple/create-genetic-algorithm 0)]

      (algorithm/advance algorithm simulation-parameters)
      (is (zero? @counter)))))
