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

(defn create-random-challenge [max-size]
  (pick-random-words-sequence (clean-text (slurp (clojure.java.io/resource "hamlet.txt")))
                              max-size
                              rand-int))

(defn simple-distance [s1 s2]
  (+ (Math/abs (- (count s1) (count s2)))
     (apply + (map (fn [c1 c2]
                     (if (not= c1 c2) 1 0)) s1 s2))))
