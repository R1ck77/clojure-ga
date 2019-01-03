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
  ([N rand-f]
   (if (> N 1)
     (vec (repeatedly N #(vector (rand-f) (rand-f))))
     (throw (IllegalArgumentException. "N must be > 1")))))

(defn- distance [[ax ay] [bx by]]
  (let [x-diff (- ax bx)
        y-diff (- ay by)]
    (Math/sqrt (+ (* x-diff x-diff)
                  (* y-diff y-diff)))))

(defn travel-length [cities order]
  (apply + (map #(apply distance %)
                (partition 2 1 (map #(get cities %)  order)))))

(defn create-crossover-operator [crossover-p rand-f rand-nth-f]
  (let [crossover-function (fn [a b]                                        
                             (utils/cross-sequences a b rand-nth-f))]
    (crossover/create-classic-crossover crossover-function
                                        crossover-p
                                        rand-f)))

(defn exchange-elements [chromosome rand-int-f]
  (let [a (rand-int-f (count chromosome))
        a-element (get chromosome a)
        b (rand-int-f (count chromosome))
        b-element (get chromosome b)]
    (assoc (assoc chromosome a b-element) b a-element)))

(defn create-mutation-operator
  ([mutation-p rand-f rand-int-f]  
   (let [mutation-f (fn [chromosome]
                      (let [rand-num (rand-f)]
                        (if (< rand-num mutation-p)
                               (exchange-elements chromosome rand-int-f)
                               chromosome)))]
     (mutation/->SimpleMutation mutation-f))))

(defn- remove-from-collection [x-int rand-int-f]
  (let [index (rand-int-f (count x-int))]
    (vector (get x-int index)
            (vec (concat (subvec x-int 0 index)
                     (subvec x-int (inc index)
                             (count x-int)))))))

(defn new-random-route [N rand-int-f]
  (let [seed (atom (vec (range N)))
        result (atom [])]
    (while (not (empty? @seed))
      (let [[next next-seed] (remove-from-collection @seed rand-int-f)]
        (reset! seed next-seed)
        (swap! result #(conj % next))))
    @result))

(defn create-tournament-selector [rank cities rand-nth-f]
  (tournament/create-selector rank
                              (fn [route] (- (travel-length cities (vec route))))
                              rand-nth-f))

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
  [cities generations population-size {:keys [mutation-p crossover-p tournament-rank
                                              rand-f rand-nth-f rand-int-f] :as parameters}]
  (let [evolver (simple/->SimpleGA (create-tournament-selector tournament-rank cities rand-nth-f)
                                   (create-crossover-operator crossover-p rand-f rand-nth-f)
                                   (create-mutation-operator mutation-p rand-f rand-int-f))]
    (println "TODO: put the challenge here somehow")
    (let [simulation (simple/->SimpleSimulation evolver (create-countdown cities generations))
          population (repeatedly population-size #(new-random-route (count cities) rand-int-f))]
      (let [best-scored-results (sort-by first (map #(vector (travel-length cities %) %) (simple/evolve-while simulation population)))
            best-results (map second best-scored-results)
            best-scores (take 10 (map first best-scored-results))]
        (println "* Best scores")
        (dorun (map println (reverse best-scores)))
        (println "* Best solution")
        (dorun (map #(let [city (get cities %)]
                       (println (first city) (second city) ))
                    (first best-results)))))))

(defn simple-simulation [N generations population-size]
  (simulation (gen-cities N rand)
              generations
              population-size
              {:mutation-p tsp-mutation-probability
               :crossover-p tsp-crossover-probability
               :tournament-rank tsp-tournament-selector-rank
               :rand-f rand
               :rand-int-f rand-int
               :rand-nth-f rand-nth}))
