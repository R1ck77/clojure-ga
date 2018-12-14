(ns clojure-ga.simple
  (:require [clojure-ga.algorithm :as algorithm]))

(defrecord SimpleGeneticAlgorithm [end-condition])

(defprotocol GeneticAlgorithm
  (select [this])
  (breed [this])
  (terminate? [this]))

(extend-type SimpleGeneticAlgorithm
  GeneticAlgorithm
  (select [this] true)
  (breed [this] true)
  (terminate? [this] false)
  algorithm/Algorithm
  (advance [this simulation] false))

(defn create-genetic-algorithm
  ([generations]
   (create-genetic-algorithm generations 0))
  ([generations current-generation]
   (->SimpleGeneticAlgorithm (fn [] (< current-generation generations)))))



