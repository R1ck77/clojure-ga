(ns clojure-ga.crossover
  (:require [clojure.zip :as zip]))

(defprotocol crossover
  (combine [this population]
    "operate a crossover operator on all pairs in the population" ))

(defrecord simplecrossover [crossover-f])

(extend-type simplecrossover
  crossover
  (combine [this population]
    (doall
     (mapcat #(apply (get this :crossover-f) %)
             (partition 2 2 population)))))

(defn create-classic-crossover [crossover-operator probability random-f]
  (let [crossover-f (fn [a b]
                      (if (< (random-f) probability)
                        (crossover-operator a b)
                        (vector a b)))]
    (->simplecrossover crossover-f)))

(defn create-1p-vector-crossover [probability random-int-f random-f]
  (create-classic-crossover (fn [a b]
                              (let [point-a (random-int-f (count a))
                                    a-halves (split-at point-a a)
                                    point-b (random-int-f (count b))
                                    b-halves (split-at point-b b)]
                                (vector (vec (concat (first b-halves) (second a-halves)))
                                        (vec (concat (first a-halves) (second b-halves))))))
                            probability random-f))

(defn count-split-points [form]
  (inc
   (if (seq? form)
     (apply + (map count-split-points (rest form)))
     0)))

(defn split-at-point [loc placeholder]
  (let [left-loc (zip/edit loc (fn [node] placeholder))]
    (vector (zip/root left-loc)
            (zip/node loc))))

;;; i miss an intermediate step: a version of count-split-points that returns pairs!!!

(defn- zip-walk [loc placeholder acc]
  (if (zip/branch? loc)
    (zip-walk (-> loc zip/down zip/right) placeholder (conj acc (split-at-point loc placeholder)))
    (let [new-acc (conj acc (split-at-point loc placeholder))
          next (zip/right loc)]
      (if next
        (zip-walk next placeholder new-acc)
        new-acc))))

(defn all-split-points
  [form placeholder]
  (zip-walk (zip/seq-zip form) placeholder []))

