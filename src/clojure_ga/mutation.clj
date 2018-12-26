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
    (pmap (get this :mutation-f) population)))

(defn create-vector-mutation [mutation-f probability random-f]
  (let [mutation-f (fn [element]
                     (if (< (random-f) probability)
                       (mutation-f element)
                       element))]
    (->SimpleMutation mutation-f)))

(defn- goto-next-location [loc stop]
  "Returns the next location or stop if there isn't any"
  (if (zip/branch? loc)
    (-> loc zip/down zip/right)
    (or (zip/right loc) stop)))

(defn- tree-traverse-f [loc mutation-f condition-f]
  (let [stop (gensym)]
    (let [mutated-location (if (condition-f)
                             (zip/edit loc mutation-f)
                             loc)
          next-location (goto-next-location mutated-location stop)]
      (if (identical? stop next-location)
        (zip/root mutated-location)
        (recur next-location mutation-f condition-f)))))

(defn- tree-mutation-f [mutation-f condition-f]
  (fn [chromosome]
    (tree-traverse-f (zip/seq-zip chromosome)
                     mutation-f
                     condition-f)))

(defn create-tree-mutation [mutation-f probability random-f]
  (let [condition-f #(< (random-f) probability)
        tree-mutation-f (tree-mutation-f mutation-f condition-f)]
    (->SimpleMutation tree-mutation-f)))
