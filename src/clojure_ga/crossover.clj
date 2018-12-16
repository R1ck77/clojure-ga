(ns clojure-ga.crossover)

(defprotocol Crossover
  (combine [this population]
    "Operate a crossover operator on all pairs in the population" ))

(defrecord SimpleCrossover [crossover-f])

(extend-type SimpleCrossover
  Crossover
  (combine [this population]
    (doall
     (mapcat #(apply (get this :crossover-f) %)
             (partition 2 2 population)))))
