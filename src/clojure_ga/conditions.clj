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

(defrecord SideEffectCondition [end-condition side-effect-f])

;;; TODO/FIXME all nice and good, but frankly a simple function for the condition will do…
(defn create-side-effect-condition [condition side-effect-f]
  (->SideEffectCondition condition side-effect-f))

(extend-protocol EndCondition
  SideEffectCondition
  (should-end? [this population]
    ((:side-effect-f this) population)
    (should-end? (:end-condition this) population)))

