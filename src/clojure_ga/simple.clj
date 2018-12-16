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
