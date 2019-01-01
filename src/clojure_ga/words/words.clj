(ns clojure-ga.words.words
  (:require [clojure-ga.simple :as simple]
            [clojure-ga.tournament-selection :as tournament]
            [clojure-ga.crossover :as crossover]
            [clojure-ga.mutation :as mutation]
            [clojure-ga.conditions :as conditions]
            [clojure-ga.statistics :as statistics]))

(def words-crossover-probability 0.2)
(def words-mutation-probability 0.01)
(def words-tournament-selector-rank 2)

(def words-seed-max-size 5)

(defn words-seq
  "Return a list of word start and end indexes"
  ([text] (words-seq text 0))
  ([text base]
   (let [matcher (re-matcher #"[^\s]+" text)]
     (if (.find matcher)
       (let [match-start (.start matcher)
             match-end (.end matcher)]
         (lazy-seq
          (cons (vector (+ base match-start) (+ base match-end))
                (words-seq (.substring text match-end) (+ base match-end)))))
       '()))))

(defn- get-words-range [words-indices]
  (vector (or (first (first words-indices)) 0)
   (or (last (last words-indices)) 0)))

(defn pick-random-words-sequence [text max-size rand-int]
  {:post (<= (count %) max-size)}
  (let [start (rand-int (count text))
        max-end (+ start max-size)]
    (let [start-sequence (drop-while #(< (first %) start)
                                     (words-seq text))
          first-word-index (or (first (first start-sequence)) 0)]
      (apply #(.substring text % %2)
             (get-words-range (take-while #(<= (- (second %) first-word-index) max-size)
                                          start-sequence))))))

(defn clean-text
  "Remove non letter/space characters"
  [text]
  (clojure.string/replace text
                          #"[^\p{L}\s]+"
                          " "))

(defn read-source-text []
  (clean-text (slurp (clojure.java.io/resource "hamlet.txt"))))

(defn extract-text-characters [clean-text]
  (seq (reduce conj #{} clean-text)))

(defn create-random-challenge [clean-text max-size]
  (pick-random-words-sequence clean-text
                              max-size
                              rand-int))

(defn- from-start-distance [s1 s2]
  (+ (Math/abs (- (count s1)
                  (count s2)))
     (apply + (map (fn [c1 c2]
                     (if (not= c1 c2) 1 0))
                   s1 s2))))

(defn best-distance [s1 s2]
  (apply min
         (map #(+ % (from-start-distance (drop % s1) s2))
              (range (inc (count s1))))))

(defn- create-score-function [challenge]
  (fn [chromosome]
    (-
     (best-distance challenge chromosome))))

(defn create-tournament-selector
  [challenge]
  (tournament/create-selector words-tournament-selector-rank
                              (create-score-function challenge)
                              rand-nth))

(defn create-words-crossover-operator []
  (crossover/create-1p-vector-crossover words-crossover-probability
                                        rand-int rand))

(defn create-words-mutation-operator [characters]
  (mutation/create-vector-mutation (fn [character]
                                      (rand-nth characters))
                                    words-mutation-probability
                                    rand))

(defn- population-stats [challenge population]
  (let [scores (map (create-score-function challenge) population)]
    (apply #(format "%s %s max: %s" % %2 (apply max scores))
           (map double (statistics/mean-std-dev scores)))))

(defn create-countdown [challenge counter]
  (let [current (atom 0)]
    (conditions/create-side-effect-condition-f (conditions/create-counter-condition-f counter)
                                               (fn [population]
                                                 (println "Generation"
                                                          (swap! current inc)
                                                          (population-stats challenge population))))))

(defn- prepare-data [max-challenge-size]
  (let [clean-text (read-source-text)]
    {:challenge (create-random-challenge clean-text max-challenge-size)
     :characters (extract-text-characters clean-text)}))

(defn create-random-word [characters max-size]
  (let [size (inc (rand-int max-size))]
    (vec (take size (repeatedly #(rand-nth characters))))))

(defn simulation
  [max-challenge-size generations simulation-size]
  (let [data (prepare-data max-challenge-size)
        characters (:characters data)
        challenge (:challenge data)
        evolver (simple/->SimpleGA (create-tournament-selector challenge)
                                   (create-words-crossover-operator)
                                   (create-words-mutation-operator characters))]
    (println (str "*** The challenge is: " challenge))
    (let [simulation(simple/->SimpleSimulation evolver (create-countdown challenge generations))
          population (take simulation-size (repeatedly #(create-random-word characters words-seed-max-size)))]
          (map #(vector (best-distance challenge %) %)         
               (simple/evolve-while simulation
                                    population)))))
