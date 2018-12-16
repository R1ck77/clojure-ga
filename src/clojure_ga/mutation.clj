(ns clojure-ga.mutation)

(defprotocol Mutation
  (mutate [this population]
    "Apply a mutation operator on all pairs of the population"))

(defrecord SimpleMutation [mutation-f])

(extend-type SimpleMutation
  Mutation
  (mutate [this population]
    (map (get this :mutation-f) population)))
