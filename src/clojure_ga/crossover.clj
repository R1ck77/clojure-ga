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

(defn create-classic-crossover [crossover-operator probability random-f]
  (let [crossover-f (fn [a b]
                      (if (< (random-f) probability)
                        (crossover-operator a b)
                        (vector a b)))]
    (->SimpleCrossover crossover-f)))

(defn create-1p-vector-crossover [probability random-int-f random-f]
  (create-classic-crossover (fn [a b]
                       (let [point-a (random-int-f (count a))
                             a-halves (split-at point-a a)
                             point-b (random-int-f (count b))
                             b-halves (split-at point-b b)]
                         (vector (vec (concat (first b-halves) (second a-halves)))
                                 (vec (concat (first a-halves) (second b-halves))))))
                            probability random-f))

