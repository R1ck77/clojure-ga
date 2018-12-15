(ns clojure-ga.crossover)

(defn crossover [population cross-function]
  (doall
   (mapcat #(apply cross-function %)
           (partition 2 2 population))))
