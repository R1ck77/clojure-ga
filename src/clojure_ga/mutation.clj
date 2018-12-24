(ns clojure-ga.mutation
  (require [clojure.walk :as walk]
           [clojure.zip :as zip]))

(defprotocol Mutation
  (mutate [this population]
    "Apply a mutation operator on all chromosomes in the population"))

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

(defn- tree-mutation-f [mutation-f probability random-f]
  (fn [chromosome]
    (let [loc (zip/seq-zip chromosome)]
      (if (zip/branch? loc))
      )))

(defn create-tree-mutation [mutation-f probability random-f]
  (let [tree-mutation-f (tree-mutation-f mutation-f probability random-f)]
    (->SimpleMutation tree-mutation-f)))
