(ns clojure-ga.simple
  (:require [clojure-ga.fitness-proportionate-selection :as selection]
            [clojure-ga.crossover :as crossover]
            [clojure-ga.mutation :as mutation]))

(defn evolve [population {:keys [:fitness-f :cross-f :mutation-f :random-f]}]  
  (mutation/mutation (selection/select (count population) population fitness-f random-f)
                     mutation-f))
