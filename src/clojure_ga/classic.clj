(ns clojure-ga.classic
  (:require [clojure-ga.engine :as engine])
  (:import [clojure-ga.engine]))

(defrecord ClassicGeneticAlgorithm [config population])

(defprotocol GeneticAlgorithm
  (select [this])
  (breed [this])
  (terminate? [this]))

(extend-type ClassicGeneticAlgorithm
  GeneticAlgorithm
  (select [this] true)
  (breed [this] true)
  (terminate? [this] false)
  engine/Algorithm
  (advance [this simulation] false))



