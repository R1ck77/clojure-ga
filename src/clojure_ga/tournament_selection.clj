(ns clojure-ga.tournament-selection
  (:require [clojure-ga.selector :as selector]))

(defrecord TournmentSelector [rank fitness-function])

(defn create-selector [rank fitness-function]
  (if (< rank 1)
    (throw (IllegalArgumentException. "The rank must be positive"))
    (->TournmentSelector rank fitness-function)))

(extend-type TournmentSelector
  selector/Selector
  (select [this population]
    []))
