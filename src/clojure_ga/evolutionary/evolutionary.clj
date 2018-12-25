(ns clojure-ga.evolutionary.evolutionary
  (:require [clojure.zip :as zip])
  (:import [java.lang Math]))

(def nesting-probability 0.8)

(def unary-operators ['√ 'sin 'cos '𝑒])

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

(defn create-binary-expression [variables]
  (list (rand-nth binary-operators)
        (gen-term variables)
        (gen-term variables)))

(defn create-unary-expression [variables]
  (list (rand-nth unary-operators)
        (gen-term variables)))


(defn pick-argument [variables random-f rand-nth]
  (if (< (random-f) nesting-probability)
    ((rand-nth [#(create-binary-expression variables) #(create-unary-expression variables)]))
    ((rand-nth [#(rand-nth variables) #(gen-constant)]))))

(defn gen-term [variables]
  (pick-argument variables rand rand-nth))
