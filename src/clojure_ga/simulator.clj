(ns clojure-ga.simulator)

(defprotocol Simulator
  (step [this] "execute a simulation step. Returns a new simulation"))

