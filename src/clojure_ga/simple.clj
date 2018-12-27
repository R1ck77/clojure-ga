(ns clojure-ga.simple
  (:require [clojure-ga.fitness-proportionate-selection :as selection]
            [clojure-ga.crossover :as crossover]
            [clojure-ga.mutation :as mutation]))

(defprotocol Evolver
  (evolve [this population] "evolve a single step of a population"))

(defrecord SimpleGA [selector crossover mutation])

(extend-type SimpleGA
  Evolver
  (evolve [this population]
    (mutation/mutate (get this :mutation)
                     (crossover/combine (get this :crossover)
                                        (selection/select (get this :selector) population)))))

(defprotocol Simulator
  (evolve-while [this population] "evolve multiple steps for a population"))

(defrecord SimpleSimulation [evolver condition-f])

(extend-type SimpleSimulation
  Simulator
  (evolve-while [this population]
    (if ((get this :condition-f) population)
      (recur this (evolve (get this :evolver) population))
      population)))
