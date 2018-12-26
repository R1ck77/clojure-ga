(ns clojure-ga.evolutionary.evolutionary
  (:require [clojure.zip :as zip]
            [clojure.walk :as walk]
            [clojure-ga.fitness-proportionate-selection :as fitness]
            [clojure-ga.simple :as simple]
            [clojure-ga.crossover :as crossover]
            [clojure-ga.mutation :as mutation])
  (:import [java.lang Math]))

(def max-depth 10)

(def unary-operators ['√ 'sin 'cos '𝑒])

(def symbol-to-function {'√ 'Math/sqrt
                         '𝑒 'Math/exp
                         'sin 'Math/sin
                         'cos 'Math/cos})

(def binary-operators ['+ '- '* '/])

(def constants-distribution (flatten
                             (vector (repeat 32 1)
                                     (repeat 16 2)
                                     (repeat 8 4)
                                     (repeat 4 8)
                                     (repeat 2 16)
                                     (repeat 1 32))))

(defn gen-constant []
  "Create a constant with equally distributed positive and negative values using 'constants-distributions' as seed"
  (* (rand-nth [-1 1])
     (rand-nth constants-distribution)))

(def gen-term)

(defn create-binary-expression [variables depth]
  (list (rand-nth binary-operators)
        (gen-term variables (inc depth))
        (gen-term variables (inc depth))))

(defn create-unary-expression [variables depth]
  (list (rand-nth unary-operators)
        (gen-term variables (inc depth))))


(defn pick-argument [variables random-f rand-nth depth]
  (if (> (random-f) (/ depth max-depth))
    ((rand-nth [#(create-binary-expression variables depth) #(create-unary-expression variables depth)]))
    ((rand-nth [#(rand-nth variables) #(gen-constant)]))))

(defn gen-term
  ([variables]
   (gen-term variables 0))
  ([variables depth]
   (pick-argument variables rand rand-nth depth)))

(defn- replace-symbols [form variables-associations]
  (walk/postwalk (fn [element]
                   (or (get symbol-to-function element)
                       (get variables-associations element)
                       element))
                 form))

(defn- create-associations [variables]
  (into {} (map #(vector % (gensym)) variables)))

(defn expression-to-function [formula & variables]
  (let [variables-associations (create-associations variables)]
    (eval
     `(fn ~(vec (map variables-associations variables))
        (try
          ~(replace-symbols formula variables-associations)
          (catch ArithmeticException e# Double/NaN))))))

(defn debug-print-to-file [function-1d file from to delta]
  (dorun
   (map #(let [value (function-1d %)]
           (spit file (str % " " value "\n") :append true ))
        (range from to delta))))

(def default-max-error 1e6)

(defn- eval-function-at-point [function point]
  (- (first point)
     (apply function (rest point))))

(defn- error-at-point [function point max-error]
  (min max-error
       (Math/abs (eval-function-at-point function point))))

(defn evaluate-chromosome [chromosome points variables max-error]
  (let [total-error (* (count points) max-error)
        function (apply (partial expression-to-function chromosome) variables)]
   (reduce #(- % (error-at-point function %2 max-error)) total-error points)))

(defn- create-fitness-selector
  ([points variables] (create-fitness-selector points variables default-max-error))
  ([points variables max-error]
   (fitness/->FitnessSelector #(evaluate-chromosome % points variables max-error) rand)))

(defn- create-argument-mutation-f [variables]
  (fn [_]
    (gen-term variables)))

(defn variables-from-points [points]
  (let [dimensions (dec (count (first points)))]
    (vec (map (comp keyword str) (range dimensions)))))

(defn create-simulator [points max-error]
  (let [variables (variables-from-points points)]
    (simple/->SimpleGA (create-fitness-selector points variables default-max-error)
                       (crossover/create-1p-tree-crossover 0.2 rand-int rand)
                       (mutation/create-tree-mutation (create-argument-mutation-f variables) 0.01 rand))))

(defn- are-points-valid? [points]
  (and (> (count points) 0)
       (> 0 (count (first points)))))

(defn simulation
  [points max-error]
  {:pre [(are-points-valid? points)]}
  (let [evolver (create-simulator points max-error)]
    ))
