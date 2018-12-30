(ns clojure-ga.conditions-test
  (:require [clojure.test :refer :all]
            [clojure-ga.conditions :as conditions]))

(def fake-population [:a :b :c :d :e])
(def other-fake-population [1 2 3 4])

(deftest test-create-count-end-condition
  (testing "0 times condition returns always false"
    (let [counter-condition (conditions/create-count-end-condition 0)]
      (is (not (or (conditions/should-end? counter-condition fake-population)
                   (conditions/should-end? counter-condition fake-population)
                   (conditions/should-end? counter-condition fake-population)
                   (conditions/should-end? counter-condition fake-population))))))
    (testing "1 times condition returns true, then false"
      (let [counter-condition (conditions/create-count-end-condition 1)]
        (is (conditions/should-end? counter-condition fake-population))
        (is (not (or (conditions/should-end? counter-condition fake-population)
                   (conditions/should-end? counter-condition fake-population)
                   (conditions/should-end? counter-condition fake-population)
                   (conditions/should-end? counter-condition fake-population))))))
  (testing "a negative count always returns false"
    (let [counter-condition (conditions/create-count-end-condition -1)]
      (is (not (or (conditions/should-end? counter-condition fake-population)
                   (conditions/should-end? counter-condition fake-population)
                   (conditions/should-end? counter-condition fake-population)
                   (conditions/should-end? counter-condition fake-population)))))))

(deftest test-create-side-effect-condition
  (testing "when used as condition, invokes the side effect function and returns true if the condition is true"
    (let [side-effect-invoked (atom false)
          side-effect-condition (conditions/create-side-effect-condition (reify conditions/EndCondition (should-end? [this population] true))
                                                                         (fn [population] (reset! side-effect-invoked fake-population)))]      
      (is (conditions/should-end? side-effect-condition fake-population))
      (is (identical? fake-population @side-effect-invoked))))
  (testing "when used as condition, invokes the side effect function and returns false if the condition is false"
    (let [side-effect-invoked (atom false)
          side-effect-condition (conditions/create-side-effect-condition (reify conditions/EndCondition (should-end? [this population] false))
                                                                         (fn [population] (reset! side-effect-invoked fake-population)))]      
      (is (not (conditions/should-end? side-effect-condition fake-population)))
      (is (identical? fake-population @side-effect-invoked))))
  (testing "both the side effect and the condition are called on the population each time"
    (let [condition-counter (atom nil)
          side-effect-counter (atom nil)
          expected-result (atom false)
          counter-condition (reify conditions/EndCondition
                              (should-end? [this population]
                                (reset! condition-counter population)
                                @expected-result))
          side-effect-f #(reset! side-effect-counter %)
          side-effect-condition (conditions/create-side-effect-condition counter-condition
                                                                         side-effect-f)]
      (is (= false (conditions/should-end? side-effect-condition fake-population)))
      (is (identical? @condition-counter fake-population))
      (is (identical? @side-effect-counter fake-population))
      (reset! expected-result true)
      (is (= true (conditions/should-end? side-effect-condition other-fake-population)))
      (is (identical? @condition-counter other-fake-population))
      (is (identical? @side-effect-counter other-fake-population)))))
