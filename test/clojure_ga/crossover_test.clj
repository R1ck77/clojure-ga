(ns clojure-ga.crossover-test
  (:require [clojure.test :refer :all]
            [clojure-ga.utils :as utils]
            [clojure-ga.crossover :as crossover]))

(deftest not-enough-elements
  (testing "doesn't invoke the cross operator on an empty population"
    (crossover/crossover []
                         {:random-f #(throw (IllegalStateException. "Unexpected execution of the random generator"))
                          :p-cross 1
                          :cross-f (fn [_ _] (is false "operator invoked but not expected"))})
    (is true))
  (testing "returns an empty vector on an empty population"
    (is (= []
           (crossover/crossover []
                                {:random-f #(throw (IllegalStateException. "Unexpected execution of the random generator"))
                                 :p-cross 1
                                 :cross-f (fn [_ _] (is false "operator invoked but not expected"))}))))  
  (testing "doesn't invoke the cross operator on a population of one element"
    (crossover/crossover [:chromosome]
                         {:random-f #(throw (IllegalStateException. "Unexpected execution of the random generator"))
                          :p-cross 1
                          :cross-f (fn [_ _] (is false "operator invoked but not expected"))}))
  (testing "returns an empty vector on a population of one element"
    (is (= []
           (crossover/crossover [:chromosome]
                                {:random-f #(throw (IllegalStateException. "Unexpected execution of the random generator"))
                                 :p-cross 1
                                 :cross-f (fn [_ _] (is false "operator invoked but not expected"))})))))

(defn- crossover-on-two-elements [counter p-cross expected-random-value]
  (let [conversion {:chromosome1 :result1
                    :chromosome2 :result2}]
    (crossover/crossover [:chromosome1 :chromosome2]
                         {:random-f #(identity expected-random-value)
                          :p-cross p-cross
                          :cross-f (fn [a b]
                                     (swap! counter inc)
                                     (is (and (= :chromosome1 a)
                                              (= :chromosome2 b)))
                                     (vector (get conversion a)
                                             (get conversion b)))})))
(deftest one-pair
  (testing "in a two chromosomes population with p cross 1, crossover applies the operator once on the chromosomes"
    (let [counter (atom 0)]
      (crossover-on-two-elements counter 1 0.5)
      (is (= 1 @counter) "Crossover operator not executed the expected number of times")))
  (testing "in a two chromosomes population with p cross a, crossover applies the operator only if the random generator return b <=a"
    (let [counter (atom 0)]
      (crossover-on-two-elements counter 0.3 0.2)    
      (is (= 1 @counter) "Crossover operator not executed the expected number of times")
      (reset! counter 0)

      (crossover-on-two-elements counter 0.3 0.4)    
      (is (= 0 @counter) "Crossover operator not executed the expected number of times")
      (reset! counter 0)

      (crossover-on-two-elements counter 0.3 0.3)    
      (is (= 1 @counter) "Crossover operator not executed the expected number of times")
      (reset! counter 0)

      (crossover-on-two-elements counter 0 0)
      (is (= 1 @counter) "Crossover operator not executed the expected number of times")
      (reset! counter 0)))
  (testing "for two chromosomes returns the output of the crossover function when the crossover is applied"
    (let [counter (atom 0)]
      (is (= [:result1 :result2]
             (crossover-on-two-elements counter 1 1)))))
  (testing "for two chromosomes returns the original pair when the crossover is not applied"
    (let [counter (atom 0)]
      (is (= [:chromosome1 :chromosome2]
             (crossover-on-two-elements counter 0 1))))))

(deftest multiple-values
  (testing "general case: various probabilities and multiple chromosomes"
    (is (= [11 -8 3 4 5 6 17 -2]
           (crossover/crossover [1 2 3 4 5 6 7 8]
                                {:random-f (utils/create-iterator [0.2 0.8 0.8 0.1])
                                 :p-cross 0.5
                                 :cross-f (fn [a b] (vector (+ a 10) (- b 10)))}))))
  (testing "general case: various probabilities and multiple chromosomes, odd elements"
    (is (= [11 -8 3 4 5 6 17 -2]
           (crossover/crossover [1 2 3 4 5 6 7 8 9]
                                {:random-f (utils/create-iterator [0.2 0.8 0.8 0.5])
                                 :p-cross 0.5
                                 :cross-f (fn [a b] (vector (+ a 10) (- b 10)))})))))

