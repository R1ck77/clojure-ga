(ns clojure-ga.tsp.demo
  (:require [clojure-ga.tsp.utils :as utils]
            [clojure-ga.simple :as simple]            
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
  (let [crossover-function (fn [[a b]]                                        
                             (utils/cross-sequences a b rand-nth))]
    (crossover/create-classic-crossover crossover-function
                                        tsp-crossover-probability
                                        rand)))

(defn exchange-elements [chromosome rand-int]
  (let [a (rand-int (count chromosome))
        a-element (get chromosome a)
        b (rand-int (count chromosome))
        b-element (get chromosome b)]
    (assoc (assoc chromosome a b-element) b a-element)))

(defn create-mutation-operator
  ([] (create-mutation-operator rand rand-int))
  ([rand rand-int]  
   (let [mutation-f (fn [chromosome]
                      (if (< (rand) tsp-mutation-probability)
                        (exchange-elements chromosome rand-int)
                        chromosome))]
     (mutation/->SimpleMutation mutation-f))))
