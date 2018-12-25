(ns clojure-ga.evolutionary.evolutionary
  (:require [clojure.zip :as zip]
            [clojure.walk :as walk])
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

(defn- replace-symbols [form]
  (walk/postwalk (fn [element]
                   (or (get symbol-to-function element) element))
                 form))

(defn- replace-variables [form variables-associations]
  (walk/postwalk (fn [element]
                   (or (get variables-associations element) element))
                 form))

(defn- create-associations [variables]
  (into {} (map #(vector % (gensym)) variables)))

(defn expression-to-function [formula & variables]
  (let [variables-associations (create-associations variables)]
    (eval
     (list 'fn (vec (map variables-associations variables))
           (replace-variables (replace-symbols formula)
                              variables-associations)))))
