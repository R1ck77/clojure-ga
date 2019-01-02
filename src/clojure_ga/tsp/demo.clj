(ns clojure-ga.tsp.demo
  (:require [clojure-ga.tsp.utils :as utils]
            [clojure-ga.simple :as simple]            
            [clojure-ga.tournament-selection :as tournament]
            [clojure-ga.crossover :as crossover]
            [clojure-ga.mutation :as mutation]
            [clojure-ga.conditions :as conditions]
            [clojure-ga.statistics :as statistics]))

(def tsp-crossover-probability 0.2)
(def tsp-mutation-probability 0.01)
(def tsp-tournament-selector-rank 2)

(defn gen-cities
  ([N] (gen-cities N rand))
  ([N rand]
   (if (> N 1)
     (vec (repeatedly N #(vector (rand) (rand))))
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
  (let [crossover-function (fn [a b]                                        
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

(defn- remove-from-collection [x-int rand-int]
  (let [index (rand-int (count x-int))]
    (vector (get x-int index)
            (vec (concat (subvec x-int 0 index)
                     (subvec x-int (inc index)
                             (count x-int)))))))

(defn new-random-route [N rand-int]
  (let [seed (atom (vec (range N)))
        result (atom [])]
    (while (not (empty? @seed))
      (let [[next next-seed] (remove-from-collection @seed rand-int)]
        (reset! seed next-seed)
        (swap! result #(conj % next))))
    @result))

(defn create-tournament-selector [cities]
  (tournament/create-selector tsp-tournament-selector-rank
                              (fn [route] (- (travel-length cities (vec route))))
                              rand-nth))

;;; TODO/FIXME repeated in the words demo
(defn- population-stats [scores]
  (apply #(format "%s %s min: %s" % %2 (apply min scores))
         (map double (statistics/mean-std-dev scores))))

(defn create-countdown [cities generations]
  (let [current (atom 0)
        counter-f (conditions/create-counter-condition-f generations)]
    (fn [population]
      (let [scores (map #(travel-length cities %) population)]        
        (println (swap! current inc) (population-stats scores))
        (counter-f population)))))

(defn simulation
  [N generations population-size]
  (let [cities (gen-cities N rand)
        evolver (simple/->SimpleGA (create-tournament-selector cities)
                                   (create-crossover-operator)
                                   (create-mutation-operator))]
    (println "TODO: put the challenge here somehow")
    (let [simulation (simple/->SimpleSimulation evolver (create-countdown cities generations))
          population (repeatedly population-size #(new-random-route N rand-int))]
      (dorun (map println (reverse (take 10 (sort (map #(travel-length cities %)
                                               (simple/evolve-while simulation population))))))))))
