(ns demo
  (:require [clojure-ga.simple :as simple]
            [clojure-ga.fitness-proportionate-selection :as selection]
            [clojure-ga.crossover :as crossover]
            [clojure-ga.mutation :as mutation]))

(def x (simple/->SimpleGA (selection/->FitnessSelector #(/ 1 (+ (Math/abs (- % 10)) 1)) rand)
                          (crossover/create-classic-crossover #(vector (+ % %2) (- % %2)) 0.2 rand)
                          (mutation/->SimpleMutation #(if (< (rand) 0.01) (+ % (rand-nth [1 -1])) %))))

(def start-population (repeat 10 0))

(defn evolve [population]
                          (simple/evolve x population))

(def number-of-steps 1000)

(reduce (fn [population _]
          (evolve population))
        start-population
        (range number-of-steps))


