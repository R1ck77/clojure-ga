(ns clojure-ga.mutation)

(defprotocol Mutation
  (mutate [this population]
    "Apply a mutation operator on all pairs of the population"))

(defrecord SimpleMutation [mutation-f])

(extend-type SimpleMutation
  Mutation
  (mutate [this population]
    (map (get this :mutation-f) population)))

(defn create-vector-mutation [mutation-f probability random-f]
  (let [mutation-f (fn [element]
                     (if (< (random-f) probability)
                       (mutation-f element)
                       element))]
    (->SimpleMutation mutation-f)))
