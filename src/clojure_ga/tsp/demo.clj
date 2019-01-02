(ns clojure-ga.tsp.demo
  (:require [clojure-ga.simple :as simple]
            [clojure-ga.tournament-selection :as tournament]
            [clojure-ga.crossover :as crossover]
            [clojure-ga.mutation :as mutation]))

(def tsp-crossover-probability 0.2)
(def tsp-mutation-probability 0.01)

(defn gen-cities
  ([N] (gen-cities N rand))
  ([N rand]
   (if (> N 1)
     (repeatedly N #(vector (rand) (rand)))
     (throw (IllegalArgumentException. "N must be > 1")))))

(defn- distance [[ax ay] [bx by]]
  (let [x-diff (- ax bx)
        y-diff (- ay by)]
    (Math/sqrt (+ (* x-diff x-diff)
                  (* y-diff y-diff)))))

(defn travel-length [cities order]
  (apply + (map #(apply distance %)
                (partition 2 1 (map #(get cities %)  order)))))

(defn create-crossover-operator []
  (crossover/create-1p-vector-crossover tsp-crossover-probability rand-int rand))

(defn create-mutation-operator [cities]
  )
