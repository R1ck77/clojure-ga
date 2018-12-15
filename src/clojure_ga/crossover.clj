(ns clojure-ga.crossover)

(defn- should-apply-operator? [random-f p-cross]
  (<= (random-f) p-cross))

(defn- attempt-cross [random-f p-cross cross-f a b]
  (if (should-apply-operator? random-f p-cross)
    (cross-f a b)))

(defn crossover [population {:keys [:random-f :p-cross :cross-f]}]
  (let [cross-function (partial attempt-cross random-f p-cross cross-f)]
    (doall
     (map #(apply cross-function %)
          (partition 2 2 population)))))
