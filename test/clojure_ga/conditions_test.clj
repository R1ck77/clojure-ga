(ns clojure-ga.conditions-test
  (:require [clojure.test :refer :all]
            [clojure-ga.conditions :as conditions]))

(def fake-population [:a :b :c :d :e])
(def other-fake-population [1 2 3 4])

(deftest test-create-counter-condition-f
  (testing "0 times condition returns always false"
    (let [counter-condition-f (conditions/create-counter-condition-f 0)]
      (is (not (or (counter-condition-f fake-population)
                   (counter-condition-f fake-population)
                   (counter-condition-f fake-population)
                   (counter-condition-f fake-population))))))
    (testing "1 times condition returns true, then false"
      (let [counter-condition-f (conditions/create-counter-condition-f 1)]
        (is (counter-condition-f fake-population))
        (is (not (or (counter-condition-f fake-population)
                     (counter-condition-f fake-population)
                     (counter-condition-f fake-population)
                     (counter-condition-f fake-population))))))
  (testing "a negative count always returns false"
    (let [counter-condition-f (conditions/create-counter-condition-f -1)]
      (is (not (or (counter-condition-f fake-population)
                   (counter-condition-f fake-population)
                   (counter-condition-f fake-population)
                   (counter-condition-f fake-population)))))))

(deftest test-create-side-effect-condition-f
  (testing "when used as condition, invokes the side effect function and returns true if the condition is true"
    (let [side-effect-invoked (atom false)
          side-effect-condition-f (conditions/create-side-effect-condition-f (fn [_] true)
                                                                             (fn [population] (reset! side-effect-invoked fake-population)))]      
      (is (side-effect-condition-f fake-population))
      (is (identical? fake-population @side-effect-invoked))))
  (testing "when used as condition, invokes the side effect function and returns false if the condition is false"
    (let [side-effect-invoked (atom false)
          side-effect-condition-f (conditions/create-side-effect-condition-f (fn [_] false)
                                                                             (fn [population] (reset! side-effect-invoked fake-population)))]      
      (is (not (side-effect-condition-f fake-population)))
      (is (identical? fake-population @side-effect-invoked))))
  (testing "both the side effect and the condition are called on the population each time"
    (let [condition-counter (atom nil)
          side-effect-counter (atom nil)
          expected-result (atom false)
          counter-condition (fn [population]
                              (reset! condition-counter population)
                              @expected-result)
          side-effect-f #(reset! side-effect-counter %)
          side-effect-condition-f (conditions/create-side-effect-condition-f counter-condition
                                                                         side-effect-f)]
      (is (= false (side-effect-condition-f fake-population)))
      (is (identical? @condition-counter fake-population))
      (is (identical? @side-effect-counter fake-population))
      (reset! expected-result true)
      (is (= true (side-effect-condition-f other-fake-population)))
      (is (identical? @condition-counter other-fake-population))
      (is (identical? @side-effect-counter other-fake-population)))))
