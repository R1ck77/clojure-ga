(ns clojure-ga.tournament-selection
  (:require [clojure-ga.selector :as selector]))

(defrecord TournmentSelector [rank fitness-function rand-nth])

(defn create-selector [rank fitness-function rand-nth]
  (if (< rank 1)
    (throw (IllegalArgumentException. "The rank must be positive"))
    (->TournmentSelector rank fitness-function rand-nth)))

(defn- pick-group [population rank rand-nth]
  (take rank (repeatedly #(rand-nth population))))

(defn- pick-groups [population pick-function]
  (take (count population) (repeatedly pick-function)))

(defn- score-group [group fitness-function]
  (map #(vector % (fitness-function %)) group))

(defn- compare-scored-chromosomes [[_ best-score :as previous-best]
                                   [_ current-score :as candidate-best]]
  (if (> current-score best-score)
    candidate-best
    previous-best))

(defn- best-of-group [scored-group]
  (first
   (reduce compare-scored-chromosomes
           (first scored-group)
           scored-group)))

(defn- pick-element [population rank fitness-function rand-nth]
  (let [pick-function #(pick-group population rank rand-nth)
        score-group-function #(score-group % fitness-function)
        groups (pick-groups population pick-function)]
    (map best-of-group
         (map score-group-function groups))))

(extend-type TournmentSelector
  selector/Selector
  (select [this population]
    (let [rank (:rank this)
          fitness-function (:fitness-function this)
          rand-nth (:rand-nth this)]
      (take (count population)
            (pick-element population rank fitness-function rand-nth)))))
