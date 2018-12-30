(ns clojure-ga.conditions-test
  (:require [clojure.test :refer :all]
            [clojure-ga.conditions :as conditions]))

(def fake-population [:a :b :c :d :e])

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
