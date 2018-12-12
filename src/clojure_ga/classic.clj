(ns clojure-ga.classic
  (:require [clojure-ga.algorithm :as algorithm]))

(defrecord ClassicGeneticAlgorithm [end-condition])

(defprotocol GeneticAlgorithm
  (select [this])
  (breed [this])
  (terminate? [this]))

(extend-type ClassicGeneticAlgorithm
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
   (->ClassicGeneticAlgorithm (fn [] (< current-generation generations)))))



