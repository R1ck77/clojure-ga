(ns clojure-ga.conditions)

(defn create-counter-condition-f [count]
  (let [countdown (atom count)]
    (fn [_]
      (>= (swap! countdown dec) 0))))

(defn create-side-effect-condition-f [condition-f side-effect-f]
  (fn [population]
    (side-effect-f population)
    (condition-f population)))
