(ns clojure-ga.words.words
  (:require [clojure-ga.simple :as simple]
            [clojure-ga.fitness-proportionate-selection :as fitness]
            [clojure-ga.crossover :as crossover]
            [clojure-ga.mutation :as mutation]))

(defn words-seq
  ([text] (words-seq text '()))
  ([text previous]
   (println previous)
   (let [base (or (last (first previous)) 0)]
     (let [matcher (re-matcher #"[^\s]+" text)]
       (if (.find matcher)
         (recur (.substring text (.end matcher))
                (conj previous (vector (+ base (.start matcher)) (+ base (.end matcher)))))
         previous)))))

(defn pick-random-words-sequence [text max-size rand-int]
)
