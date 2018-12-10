(ns clojure-ga.classic-test
  (:require [clojure.test :refer :all]
            [clojure-ga.classic :as classic]
            [clojure-ga.engine :as engine])
  (:import [clojure_ga.engine PopulationProvider Simulator Algorithm]
           [clojure_ga.classic ClassicGeneticAlgorithm]))

(deftest classic-algorithm-selection
  (testing "The algorithm has a selection phase"
    (is (not (nil? (classic/select (classic/->ClassicGeneticAlgorithm nil nil)))))))

(deftest classic-algorithm-breed
  (testing "The algorithm has a breed phase"
    (is (not (nil? (classic/breed (classic/->ClassicGeneticAlgorithm nil nil)))))))

(deftest classic-algorithm-terminate?
  (testing "The algorithm decides when to stop"
    (is (not (nil? (classic/terminate? (classic/->ClassicGeneticAlgorithm nil nil)))))))

(defn- create-mock-config []
  (engine/->Config MOCK THE METHODS HERE, PUT A CHECK))

(deftest classic-algorithm-behavior
  (testing "The classic GA does nothing if it's supposed to terminate immediately"
    (create-genetic-algorithm (create-mock-config) nil 0)
    (verify no cross or mutation or fitness operator called)))
