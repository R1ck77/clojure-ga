(ns clojure-ga.words.words
  (:require [clojure-ga.simple :as simple]
            [clojure-ga.fitness-proportionate-selection :as fitness]
            [clojure-ga.crossover :as crossover]
            [clojure-ga.mutation :as mutation]))

(defn words-seq
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

(defn pick-random-words-sequence [text max-size rand-int]
)
