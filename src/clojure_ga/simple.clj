(ns clojure-ga.simple
  (:require [clojure-ga.fitness-proportionate-selection :as selection]
            [clojure-ga.crossover :as crossover]
            [clojure-ga.mutation :as mutation]))

(defprotocol Evolver
  (evolve [this population] "evolve a single step of a population"))

(defrecord SimpleGA [fitness-f crossover mutation])

(extend-type SimpleGA
  Evolver
  (evolve [this population]
    (mutation/mutate (get this :mutation)
                     (selection/select (count population)
                                       population
                                       (get this :fitness-f)
                                       rand))))
