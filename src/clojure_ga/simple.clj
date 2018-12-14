(ns clojure-ga.simple
  (:require [clojure-ga.algorithm :as algorithm]))

(defrecord SimpleGeneticAlgorithm [end-condition])

(defprotocol GeneticAlgorithm
  (breed [this population]))

(extend-type SimpleGeneticAlgorithm
  GeneticAlgorithm
  (breed [this population] true)
  algorithm/Algorithm
  (advance [this simulation]
    (while ((:end-condition this))
      (breed this nil))))

(defn create-genetic-algorithm
  ([generations]
   (create-genetic-algorithm generations (atom 0)))
  ([generations current-generation]
   (->SimpleGeneticAlgorithm (fn [] (< @current-generation generations)))))



