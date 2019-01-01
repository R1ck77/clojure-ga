(ns clojure-ga.salesman.demo
  (:require [clojure-ga.simple :as simple]
            [clojure-ga.tournament-selection :as tournament]
            [clojure-ga.crossover :as crossover]
            [clojure-ga.mutation :as mutation]))

(defn gen-cities [N]
  (throw (IllegalArgumentException. "N must be > 1")))



