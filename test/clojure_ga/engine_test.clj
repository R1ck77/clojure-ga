(ns clojure-ga.engine-test
  (:require [clojure.test :refer :all]
            [clojure-ga.engine :as engine]))

(def reasonable-default-arguments [
                                   :p-cross 0.75
                                   :p-mutation 0.01
                                   :op-cross (fn [a b] a)
                                   :op-mutation (fn [a] a)
                                   :op-generation (fn [] )])
(defn generate-parameters-without-option [name]
  (reduce into []
          (dissoc (apply hash-map reasonable-default-arguments)
                  name)))

(def missing-p-cross (generate-parameters-without-option :p-cross))
(def missing-p-mutation (generate-parameters-without-option :p-mutation))
(def missing-op-cross (generate-parameters-without-option :op-cross))
(def missing-op-mutation (generate-parameters-without-option :op-mutation))
(def missing-op-generation (generate-parameters-without-option :op-generation))

(def p-cross-too-high [:p-cross 1.75 :p-mutation 0.01 :op-cross (fn [a b] a) :op-mutation (fn [a] a)])
(def p-cross-too-low [:p-cross -0.75 :p-mutation 0.01 :op-cross (fn [a b] a) :op-mutation (fn [a] a)])
(def p-mutation-too-high [:p-cross 0.75 :p-mutation 1.01 :op-cross (fn [a b] a) :op-mutation (fn [a] a)])
(def p-mutation-too-low [:p-cross 0.75 :p-mutation -0.01 :op-cross (fn [a b] a) :op-mutation (fn [a] a)])

(deftest engine-creation
  (testing "the engine can be instantiated"
    (is (apply engine/create-engine reasonable-default-arguments)))
  (testing "missing parameters are met with an exception"
    (is (thrown? IllegalArgumentException (apply engine/create-engine missing-op-cross)))
    (is (thrown? IllegalArgumentException (apply engine/create-engine missing-op-mutation))))
  (testing "mutation and crossing probabilities have reasonable defaults"
    (is (apply engine/create-engine missing-p-cross))
    (is (apply engine/create-engine missing-p-mutation)))
  (testing "invalid probabilities are also met with an exception"
    (is (thrown? IllegalArgumentException (apply engine/create-engine p-cross-too-high)))
    (is (thrown? IllegalArgumentException (apply engine/create-engine p-cross-too-low)))
    (is (thrown? IllegalArgumentException (apply engine/create-engine p-mutation-too-high)))
    (is (thrown? IllegalArgumentException (apply engine/create-engine p-mutation-too-low))))
  (testing "accepts an optional random generator, and saves it"
    (let [random-generator :random-generator-function
          generator-enhanced-arguments (vec (concat reasonable-default-arguments [:random-generator random-generator]))]
      (is (= random-generator (:random-generator (apply engine/create-engine generator-enhanced-arguments))))))
    (testing "a default random generator is automatically provided, when not in the options"
    (is (:random-generator (apply engine/create-engine reasonable-default-arguments)))))

(deftest engine-add-first-generation-solutions
  (testing "add first generation solutions"
    (is (engine/add-first-generation-solutions {} []))))

(deftest simulation-creation
  (testing "a simulation can be created with no initial population"
    (is (= [] (:population (engine/create-simulation)))))
  (testing "a simulation can be created with an initial population"
    (let [initial-population [:a :b :c :d]]
      (is (identical? initial-population
                      (:population (engine/create-simulation initial-population)))))))
