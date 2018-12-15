(ns clojure-ga.crossover-test
  (:require [clojure.test :refer :all]
            [clojure-ga.utils :as utils]
            [clojure-ga.crossover :as crossover]))

(deftest crossover-not-enough-elements
  (testing "crossover doesn't invoke the cross operator on an empty population"
    (crossover/crossover []
                         {:random-f #(throw (IllegalStateException. "Unexpected execution of the random generator"))
                          :p-cross 1
                          :cross-f (fn [_ _] (is false "operator invoked but not expected"))})
    (is true))
  (testing "crossover returns an empty vector on an empty population"
    (is (= []
           (crossover/crossover []
                                {:random-f #(throw (IllegalStateException. "Unexpected execution of the random generator"))
                                 :p-cross 1
                                 :cross-f (fn [_ _] (is false "operator invoked but not expected"))}))))  
  (testing "crossover doesn't invoke the cross operator on a population of one element"
    (crossover/crossover [:chromosome]
                         {:random-f #(throw (IllegalStateException. "Unexpected execution of the random generator"))
                          :p-cross 1
                          :cross-f (fn [_ _] (is false "operator invoked but not expected"))}))
  (testing "crossover returns an empty vector on a population of one element"
    (is (= []
           (crossover/crossover [:chromosome]
                                {:random-f #(throw (IllegalStateException. "Unexpected execution of the random generator"))
                                 :p-cross 1
                                 :cross-f (fn [_ _] (is false "operator invoked but not expected"))})))))

(defn- crossover-on-two-elements [counter p-cross expected-random-value]
  (crossover/crossover [:chromosome1 :chromosome2]
                       {:random-f #(identity expected-random-value)
                        :p-cross p-cross
                        :cross-f (fn [a b]
                                   (swap! counter inc)
                                   (is (and (= :chromosome1 a)
                                            (= :chromosome2 b))))}))
(deftest crossover-one-pair
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
      (reset! counter 0))))
