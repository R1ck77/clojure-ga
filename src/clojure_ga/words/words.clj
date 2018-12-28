(ns clojure-ga.words.words
  (:require [clojure-ga.simple :as simple]
            [clojure-ga.fitness-proportionate-selection :as fitness]
            [clojure-ga.crossover :as crossover]
            [clojure-ga.mutation :as mutation]))

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
  (let [start (rand-int)
        max-end (+ start max-size)]
    (apply #(.substring text % %2)
           (get-words-range (take-while #(< (second %) (+ start max-end))
                                   (drop-while #(< (first %) start)
                                               (words-seq text)))))))
