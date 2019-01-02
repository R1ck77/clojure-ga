(ns clojure-ga.tsp.demo
  (:require [clojure-ga.simple :as simple]
            [clojure-ga.tournament-selection :as tournament]
            [clojure-ga.crossover :as crossover]
            [clojure-ga.mutation :as mutation]))

(defn gen-cities
  ([N] (gen-cities N rand))
  ([N rand]
   (if (> N 1)
     (repeatedly N #(vector (rand) (rand)))
     (throw (IllegalArgumentException. "N must be > 1")))))



