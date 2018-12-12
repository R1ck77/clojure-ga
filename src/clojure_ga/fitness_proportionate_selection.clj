(ns clojure-ga.fitness-proportionate-selection)

(defn select
  "Select n non distinct elements with the Roulette Method

Use the score function with the element itself to get its fitness and the random generator for the selection"
  [n population score-function random-generator]
  (if (empty? population)
    []
    (vec (take n (repeat (first (reverse population)))))))
