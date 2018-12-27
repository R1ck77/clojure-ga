(ns clojure-ga.crossover
  (:require [clojure.zip :as zip]
            [clojure.walk :as walk]))

(defprotocol Crossover
  (combine [this population]
    "operate a crossover operator on all pairs in the population" ))

(defrecord SimpleCrossover [crossover-f])

(extend-type SimpleCrossover
  Crossover
  (combine [this population]
    (doall
     (reduce #(concat % %2) []
             (map #(apply (get this :crossover-f) %)
                   (partition 2 2 population))))))

(defn create-classic-crossover [crossover-operator probability random-f]
  (let [crossover-f (fn [a b]
                      (if (< (random-f) probability)
                        (crossover-operator a b)
                        (vector a b)))]
    (->SimpleCrossover crossover-f)))

(defn create-1p-vector-crossover [probability random-int-f random-f]
  (create-classic-crossover (fn [a b]
                              (let [point-a (random-int-f (count a))
                                    a-halves (split-at point-a a)
                                    point-b (random-int-f (count b))
                                    b-halves (split-at point-b b)]
                                (vector (vec (concat (first b-halves) (second a-halves)))
                                        (vec (concat (first a-halves) (second b-halves))))))
                            probability random-f))

(defn split-at-point [loc placeholder]
  (let [left-loc (zip/edit loc (fn [node] placeholder))]
    (vector (zip/root left-loc)
            (zip/node loc))))

(defn- zip-walk [loc placeholder acc]
(let [next (zip/right loc)]
 (if (zip/branch? loc)
   (concat (zip-walk (-> loc zip/down zip/right) placeholder (conj acc (split-at-point loc placeholder)))
           (if next
             (zip-walk next placeholder [])
             []))
   (let [new-acc (conj acc (split-at-point loc placeholder))]
     (if next
       (zip-walk next placeholder new-acc)
       new-acc)))))

(defn all-split-points
  [form placeholder]
  (zip-walk (zip/seq-zip form) placeholder []))

(defn merge-slices
  "Merge two chromosome slices together

The outer part contains an insertion element that will be replaced with the inner part.

No insertion elements or multiple ones is ok"
  [outer inner insertion-element]
  (walk/postwalk (fn [node]
                        (if (= node insertion-element)
                          inner
                          node)) outer))

(defn- pick-pairs [form random-int-f placeholder]
  (let [split-points (all-split-points form placeholder)]
    (nth split-points (random-int-f (count split-points)))))

(defn create-1p-tree-crossover [probability random-int-f random-f]
  (create-classic-crossover (fn [a b]
                              (let [placeholder (gensym)
                                    first-pair (pick-pairs a random-int-f placeholder)
                                    second-pair (pick-pairs b random-int-f placeholder)]
                                (vector (merge-slices (first first-pair) (second second-pair) placeholder)
                                        (merge-slices (first second-pair) (second first-pair) placeholder))))
                            probability random-f))
