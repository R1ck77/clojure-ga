(ns clojure-ga.fitness-proportionate-selection)

(defn- sort-by-fitness
  [population score-function]
  (sort-by #(- (first %))
           (pmap (fn [chromosome]
                  (vector (score-function chromosome) chromosome))
                population)))

(defn- accumulate-weights
  [accumulator [weight chromosome]]
  (let [[last-acc-value _] (last accumulator)
        new-accumulated-difference (+ last-acc-value weight)]
    (conj accumulator (vector new-accumulated-difference chromosome))))

(defn- weighted-population-to-accumulated-weights
  [weighted-population]
  (let [last-chromosome (second (last weighted-population))]
    (reduce accumulate-weights [[0 nil]] weighted-population)))

(defn- pick-element-from-weighted-population
  "Returns the selected chromosome using the fitness-proportionate-selection with the specified random value"
  [weighted-population value]
  (let [accumulated-weights (weighted-population-to-accumulated-weights weighted-population)
        candidate-pair (first (drop-while #(>= value (first %)) accumulated-weights))
        alternative-candidate-pair (last weighted-population)]
    (second
     (or candidate-pair
         alternative-candidate-pair))))

(defn- get-total-weight [weighted-population]
  (reduce #(+ % (first %2)) 0 weighted-population))

(defn- pick-element
  "Return the selected chromosome from the population using the specified random value"
  [weighted-population score-function random-value]
  (let [total-weight (get-total-weight weighted-population)]
    (pick-element-from-weighted-population weighted-population (* total-weight random-value))))

(defn select-chromosomes
  "Select n non distinct elements with the Roulette Method

Use the score function with the element itself to get its fitness and the random generator for the selection"
  [n population score-function random-generator]
  (if (empty? population)
    []
    (let [weighted-population (sort-by-fitness population score-function)]
      (take n (repeatedly #(pick-element weighted-population score-function (random-generator)))))))

(defprotocol Selector
  (select [this population]))

(defrecord FitnessSelector [fitness-f random-f])

(extend-type FitnessSelector
  Selector
  (select [this population]
    (let [fitness-f (get this :fitness-f)
          random-f (get this :random-f)]
     (select-chromosomes (count population) population fitness-f random-f))))
