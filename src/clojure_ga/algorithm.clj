(ns clojure-ga.algorithm)

(defprotocol Algorithm
  (advance [this simulation-map]
    "Peform a step on :algorithm :config :population and returns a new map"))

