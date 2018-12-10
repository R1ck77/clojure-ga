(ns clojure-ga.engine-test
  (:require [clojure.test :refer :all]
            [clojure-ga.engine :as engine])
  (:import [clojure_ga.engine PopulationProvider Simulator Algorithm]))

(def reasonable-default-arguments [:p-cross 0.75
                                   :p-mutation 0.01
                                   :op-cross (fn [a b] a)
                                   :op-mutation (fn [a] a)
                                   :op-generation (fn [] )
                                   :op-fitness (fn [x] 0)])

(defn- generate-parameters-without-option [name]
  (reduce into []
          (dissoc (apply hash-map reasonable-default-arguments)
                  name)))

(def missing-p-cross (generate-parameters-without-option :p-cross))
(def missing-p-mutation (generate-parameters-without-option :p-mutation))
(def missing-op-cross (generate-parameters-without-option :op-cross))
(def missing-op-mutation (generate-parameters-without-option :op-mutation))
(def missing-op-generation (generate-parameters-without-option :op-generation))
(def missing-op-fitness (generate-parameters-without-option :op-fitness))

(defn- replace-option-in-parameters [option-name new-value]
  (assoc (apply hash-map reasonable-default-arguments)
         option-name new-value))

(def p-cross-too-high (replace-option-in-parameters :p-cross 1.75))
(def p-cross-too-low (replace-option-in-parameters :p-cross -0.75))
(def p-mutation-too-high (replace-option-in-parameters  :p-mutation 1.01))
(def p-mutation-too-low (replace-option-in-parameters  :p-mutation -0.01))

(deftest config-creation
  (testing "the config can be instantiated"
    (is (apply engine/create-config reasonable-default-arguments)))
  (testing "a fitness function must be provided"
    (is (thrown? IllegalArgumentException (apply engine/create-config missing-op-fitness))))
  (testing "missing parameters are met with an exception"
    (is (thrown? IllegalArgumentException (apply engine/create-config missing-op-cross)))
    (is (thrown? IllegalArgumentException (apply engine/create-config missing-op-mutation))))
  (testing "mutation and crossing probabilities have reasonable defaults"
    (is (apply engine/create-config missing-p-cross))
    (is (apply engine/create-config missing-p-mutation)))
  (testing "invalid probabilities are also met with an exception"
    (is (thrown? IllegalArgumentException (apply engine/create-config p-cross-too-high)))
    (is (thrown? IllegalArgumentException (apply engine/create-config p-cross-too-low)))
    (is (thrown? IllegalArgumentException (apply engine/create-config p-mutation-too-high)))
    (is (thrown? IllegalArgumentException (apply engine/create-config p-mutation-too-low))))
  (testing "accepts an optional random generator, and saves it"
    (let [random-generator :random-generator-function
          generator-enhanced-arguments (vec (concat reasonable-default-arguments [:random-generator random-generator]))]
      (is (= random-generator (:random-generator (apply engine/create-config generator-enhanced-arguments))))))
    (testing "a default random generator is automatically provided, when not in the options"
    (is (:random-generator (apply engine/create-config reasonable-default-arguments)))))

(deftest config-add-first-generation-solutions
  (testing "add first generation solutions"
    (is (engine/add-first-generation-solutions {} []))))

(deftest simulation-creation
  (testing "a simulation can be created with no initial population"
    (is (= [] (:population (engine/create-simulation nil nil)))))
  (testing "a simulation can be created with an initial population"
    (let [initial-population [:a :b :c :d]]
      (is (identical? initial-population
                      (:population (engine/create-simulation nil nil initial-population)))))))

(defn create-config-creating-set-values [values]
  (let [current (atom 0)]
    (engine/create-config :op-fitness (fn [_] 42)
                          :op-cross #()
                          :op-mutation #()
                          :op-generation (fn []
                                           (let [result (get values @current)]
                                             (swap! current #(+ % 1))
                                             result)))))

(deftest population-generation
  (testing "creating a new genetic instance returns a new simulation with the same config"
    (let [config (create-config-creating-set-values [])
          new-simulation (engine/addInstance (engine/create-simulation config nil))]
      (is (identical? config (:config new-simulation)))))
  (testing "a simulation can create a new genetic instance using the op-generation function"
    (let [config (create-config-creating-set-values [:new-instance :another-instance])
          new-simulation (engine/addInstance (engine/create-simulation config nil))]
      (is (= [:new-instance] (:population new-simulation)))))
  (testing "the new instance is appended to the ones already present"
    (let [config (create-config-creating-set-values [:new-instance :another-instance])]
      (is (= [:original-instance :new-instance :another-instance]
             (:population (-> (engine/create-simulation config nil [:original-instance])
                              engine/addInstance engine/addInstance)))))))

(deftest algorithm-step
  (testing "performing a simulation step returns a new simulation instance"
    (let [algorithm (reify Algorithm
                      (advance [this simulation]
                        simulation))
          simulation (engine/create-simulation :a-config algorithm)
          new-simulation (engine/step simulation)]
      (is (not (identical? simulation new-simulation)))
      (is (= :a-config (:config new-simulation)))))
  (testing "performing a simulation step is delegated to an external algorithm"
    (let [algorithm (reify Algorithm
                      (advance [this simulation]
                        simulation))]
      (is (= {:config :a-config :population [] :algorithm algorithm}
             (into {} (engine/step (engine/create-simulation :a-config algorithm))))))))


(comment (deftest end-to-end-scenario
   (testing "I can configure a genetic algorithm simulation with stock parameters"
     (engine/create-vector-based-simulation ))))
