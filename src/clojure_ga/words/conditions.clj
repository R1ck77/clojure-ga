(ns clojure-ga.conditions)

(defprotocol EndCondition
  (should-end? [this population]))

(defrecord Counter [atom-countdown])

(defn create-count-end-condition [count]
  (->Counter (atom count)))

(extend-protocol EndCondition
  Counter
  (should-end? [this _]
    (>= (swap! (:atom-countdown this) dec) 0)))

